use super::*;

impl Expression {
    pub fn try_eval_stack(
        &self,
        constexpr: bool,
        target: &mut Target,
        asm: &Assembler,
        stack: &mut Vec<(ContextStr, StackValue)>,
        args: &[(ContextStr, StackValue)],
        label_depth: &mut Vec<usize>
    ) -> Option<()> {
        //println!("evaluating: {:?}", self);
        let get_val = |stack: &mut Vec<(ContextStr, StackValue)>, target: &mut Target| {
            let (span, arg) = stack.pop().expect("unbalanced expr");
            match arg {
                StackValue::Number { value, .. } => value,
                StackValue::String(_) => {
                    target.push_error(span.clone(), 35, "Strings are not allowed in math operations".into());
                    0.0
                },
            }
        };
        for (span, i) in self.nodes.iter() {
            match i {
                ExprNode::Value(v) if constexpr => {
                    let v = match v {
                        Value::Literal { value, .. } => StackValue::Number { value: *value, origin: None },
                        Value::String(s) => StackValue::String(s.clone()),
                        Value::Label(_) => {
                            // Do not attempt to resolve labels
                            target.push_error(span.clone(), 34, "Labels are not allowed in const contexts".into());
                            return None;
                        }
                    };
                    stack.push((span.clone(), v));
                }
                ExprNode::Value(Value::Label(c)) if label_depth.contains(&c) => {
                    target.push_error(span.clone(), 0, "Self-referential label used".into());
                    return None;
                }
                ExprNode::Value(Value::Label(c)) => {
                    label_depth.push(*c);
                    //println!("getting label value {}", c);
                    let expr = asm.get_label_value(*c);
                    if expr.is_none() {
                        target.push_error(span.clone(), 0, "Can't find label".into());
                    }
                    expr?.try_eval_stack(constexpr, target, asm, stack, &[], label_depth)?;
                    label_depth.pop();
                    stack.last_mut().unwrap().1.set_origin(*c);
                }
                ExprNode::Value(v) => {
                    let v = match v {
                        Value::Literal { value, .. } => StackValue::Number { value: *value, origin: None },
                        Value::String(s) => StackValue::String(s.clone()),
                        Value::Label(_) => unreachable!()
                    };
                    stack.push((span.clone(), v));
                }
                ExprNode::Unop(op) => {
                    let arg = get_val(stack, target);
                    let value = op.exec(&span, arg, target);
                    stack.push((span.clone(), StackValue::Number { value, origin: None }));
                }
                ExprNode::Binop(op) => {
                    let arg2 = get_val(stack, target);
                    let arg1 = get_val(stack, target);
                    let value = op.exec(&span, arg1, arg2, target);
                    stack.push((span.clone(), StackValue::Number { value, origin: None }));
                }
                ExprNode::FuncArg(f) => {
                    stack.push((span.clone(), args[*f].1.clone()));
                }
                // I really do not want to increase the indent on the builtins lol
                ExprNode::Call(len, name) if target.function(&name).is_some() => {
                    let (arity, expr) = target.function(&name).unwrap();
                    let arity = *arity;
                    if *len != arity {
                        target.push_error(span.clone(), 0, format!("Wrong number of function arguments (expected {}, found {})", arity, len));
                        return None;
                    }
                    let mut args = vec![];
                    for i in 0..*len { args.insert(0, stack.pop().unwrap()) }
                    let mut expr = expr.clone();
                    // adjust span info (otherwise errors go to the function def, which is not what
                    // you want)
                    for i in expr.nodes.iter_mut() {
                        i.0.set_parent(span.clone());
                    }
                    expr.try_eval_stack(constexpr, target, asm, stack, &args[..], label_depth)?;
                }
                ExprNode::Call(len, name) => {
                    // do builtins
                    macro_rules! arity {
                        (@l $l:expr; $($dummy:expr),*) => {
                            if $l != *len {
                                target.push_error(span.clone(), 0, format!("Wrong number of function arguments (expected {}, found {})", $l, len));
                                return None;
                            } else {
                                [$({ $dummy; get_val(stack, target) }),*]
                            }
                        };
                        (1) => { arity!(@l 1; 0) };
                        (2) => { arity!(@l 2; 0, 1) };
                        (3) => { arity!(@l 3; 0, 1, 2) };
                    }
                    let number = |value| StackValue::Number { value, origin: None };
                    macro_rules! func {
                        (|$a0:ident| $b:expr) => {{ let [$a0] = arity!(1); number($b) }};
                        (|$a0:ident, $a1:ident| $b:expr) => {{ let [$a1,$a0] = arity!(2); number($b) }};
                        (|$a0:ident, $a1:ident, $a2:ident| $b:expr) => {{ let [$a2,$a1,$a0] = arity!(3); number($b) }};
                    }
                    let value = match &**name {
                        "not" => func! { |a| Unop::Not.exec(&span, a, target) },
                        "min" => func! { |a,b| a.min(b) },
                        "max" => func! { |a,b| a.max(b) },
                        "select" => func! { |cond,a,b| if cond != 0.0 { a } else { b } },
                        "datasize" => {
                            let label_name = stack.pop().expect("unbalanced expr");
                            if let StackValue::Number { origin: Some(c), .. } = label_name.1 {
                                number(asm.get_datasize(c, span.clone(), target).expect("no label for datasize?"))
                            } else {
                                target.push_error(span.clone(), 0, "This function expects a symbol, not an expression".into());
                                return None;
                            }
                        },
                        "hex" => {
                            let [arg1] = arity!(1);
                            StackValue::String(format!("{:X}", arg1 as i64))
                        },
                        "dec" => {
                            let [arg1] = arity!(1);
                            StackValue::String(format!("{}", arg1 as i64))
                        },
                        "bin" => {
                            let [arg1] = arity!(1);
                            StackValue::String(format!("{:b}", arg1 as i64))
                        },
                        "double" => {
                            let [arg1] = arity!(1);
                            StackValue::String(format!("{}", arg1))
                        },
                        _ => {
                            target.push_error(span.clone(), 0, format!("Unknown function"));
                            return None;
                        }
                    };
                    stack.push((span.clone(), value));
                }
                _ => panic!()
            }
        }
        Some(())
    }
    pub fn try_eval(&self, constexpr: bool, target: &mut Target, asm: &Assembler) -> Option<(ContextStr, StackValue)> {
        let mut stack = vec![];
        self.try_eval_stack(constexpr, target, asm, &mut stack, &[], &mut vec![])?;
        let val = stack.pop().expect("unbalanced expr");
        //println!("value: {:?}", val);
        Some(val)
    }
    pub fn try_eval_float(&self, target: &mut Target, asm: &Assembler) -> Option<f64> {
        let val = self.try_eval(false, target,asm)?;
        match val.1 {
            StackValue::Number { value, .. } => Some(value),
            _ => {
                target.push_error(val.0, 0, "Expected an expression that returns a number".into());
                None
            }
        }
    }
    pub fn try_eval_int(&self, target: &mut Target, asm: &Assembler) -> Option<u32> {
        // see asar's getnum
        let val = self.try_eval_float(target, asm)?;
        Some(int_cast(val))
    }
    pub fn eval_const(&self, target: &mut Target) -> f64 {
        let val = self.try_eval(true, target, &Assembler::new());
        match val {
            Some((_, StackValue::Number { value, .. })) => value,
            Some((c,_)) => {
                target.push_error(c, 0, "Expected an expression that returns a number".into());
                0.0
            },
            None => 0.0
        }
    }
}