use crate::ast::{
    AssignmentTarget, Block, Expr, Handler, Operator, Pattern, Predicate, Statement, Type,
    Value,
};
use owo_colors::OwoColorize;
use std::sync::Arc;

pub enum SExpr {
    Null,
    Atom(String),
    List(Vec<SExpr>),
}

pub trait ToSExpr {
    fn to_sexpr(&self) -> SExpr;
}

impl SExpr {
    /// Returns a formatted string with ANSI escape codes.
    pub fn to_formated_string(&self) -> String {
        match self {
            SExpr::Null => "nil".to_string(),
            SExpr::Atom(s) => s.clone(),
            SExpr::List(vec) => {
                let mut result = "(".bright_magenta().to_string();
                for (i, expr) in vec.iter().enumerate() {
                    if i > 0 {
                        result.push(' ');
                    }
                    result.push_str(&expr.to_formated_string());
                }
                result.push_str(&")".bright_magenta().to_string());
                result
            }
        }
    }

    /// Returns a formatted string with an option to strip ANSI control codes.
    pub fn to_formated_string_with_options(&self, strip_ansi: bool) -> String {
        let s = self.to_formated_string();
        if strip_ansi {
            strip_ansi_codes(&s)
        } else {
            s
        }
    }
}

// Helper function: strips ANSI escape sequences from a string.
fn strip_ansi_codes(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\x1b' {
            if let Some(&next) = chars.peek() {
                if next == '[' {
                    chars.next(); // skip '['
                    // skip until we find the final letter of the ANSI code (in the range '@'..='~')
                    while let Some(&ch) = chars.peek() {
                        if ('@'..='~').contains(&ch) {
                            chars.next(); // skip the letter and break out of this escape
                            break;
                        }
                        chars.next();
                    }
                    continue;
                }
            }
        }
        result.push(c);
    }
    result
}

impl ToSExpr for String {
    fn to_sexpr(&self) -> SExpr {
        SExpr::Atom(self.clone())
    }
}

impl ToSExpr for &str {
    fn to_sexpr(&self) -> SExpr {
        SExpr::Atom(self.to_string())
    }
}

impl<T: ToSExpr> ToSExpr for Option<T> {
    fn to_sexpr(&self) -> SExpr {
        match self {
            Some(x) => x.to_sexpr(),
            None => SExpr::Atom("None".to_string()),
        }
    }
}

impl<T: ToSExpr> ToSExpr for Arc<T> {
    fn to_sexpr(&self) -> SExpr {
        self.as_ref().to_sexpr()
    }
}

impl<T: ToSExpr> ToSExpr for crate::ast::Spanned<T> {
    fn to_sexpr(&self) -> SExpr {
        self.value.to_sexpr()
    }
}

impl ToSExpr for Expr {
    fn to_sexpr(&self) -> SExpr {
        use Expr::*;
        match self {
            Value(v) => SExpr::List(vec![
                SExpr::Atom("Value".bright_blue().to_string()),
                v.to_sexpr(),
            ]),
            Ask { target, value } => SExpr::List(vec![
                SExpr::Atom("Ask".bright_blue().to_string()),
                target.to_sexpr(),
                value.to_sexpr(),
            ]),
            List { elements } => {
                let mut vec = vec![SExpr::Atom("List".bright_blue().to_string())];
                for elem in elements {
                    vec.push(elem.to_sexpr());
                }
                SExpr::List(vec)
            }
            BinaryExpr { lhs, op, rhs } => SExpr::List(vec![
                SExpr::Atom("BinaryExpr".bright_blue().to_string()),
                lhs.to_sexpr(),
                op.to_sexpr(),
                rhs.to_sexpr(),
            ]),
            SliceExpr { target, index } => SExpr::List(vec![
                SExpr::Atom("SliceExpr".bright_blue().to_string()),
                target.to_sexpr(),
                index.to_sexpr(),
            ]),
        }
    }
}

impl ToSExpr for Value {
    fn to_sexpr(&self) -> SExpr {
        use Value::*;
        match self {
            Null => SExpr::Atom("null".bright_green().to_string()),
            Error(e) => SExpr::List(vec![
                SExpr::Atom("Error".bright_blue().to_string()),
                e.to_sexpr(),
            ]),
            Channel(c) => SExpr::List(vec![
                SExpr::Atom("Channel".bright_blue().to_string()),
                SExpr::Atom(format!("{:?}", c)), // using Debug output
            ]),
            List(list) => {
                let mut sexprs = vec![SExpr::Atom("List".bright_blue().to_string())];
                for v in list {
                    sexprs.push(v.to_sexpr());
                }
                SExpr::List(sexprs)
            }
            Word(w) => SExpr::Atom(w.bright_green().to_string()),
            Boolean(b) => SExpr::Atom(b.to_string().bright_green().to_string()),
            String(s) => SExpr::Atom(format!("\"{}\"", s).bright_green().to_string()),
            Int(i) => SExpr::Atom(i.to_string().bright_green().to_string()),
            Float(f) => SExpr::Atom(f.to_string().bright_green().to_string()),
            Uuid(u) => SExpr::Atom(u.to_string().bright_green().to_string()),
            Block(arc_block) => SExpr::List(vec![
                SExpr::Atom("Block".bright_blue().to_string()),
                arc_block.to_sexpr(),
            ]),
        }
    }
}

impl ToSExpr for Operator {
    fn to_sexpr(&self) -> SExpr {
        let op_str = match self {
            Operator::Add => "+",
            Operator::Subtract => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::Equal => "==",
            Operator::NotEqual => "!=",
            Operator::GreaterThan => ">",
            Operator::LessThan => "<",
        };
        SExpr::Atom(op_str.bright_yellow().to_string())
    }
}

impl ToSExpr for Statement {
    fn to_sexpr(&self) -> SExpr {
        use Statement::*;
        match self {
            BlankLine => SExpr::Atom("BlankLine".bright_blue().to_string()),
            Comment(s) => SExpr::List(vec![
                SExpr::Atom("Comment".bright_blue().to_string()),
                s.to_sexpr(),
            ]),
            Expr(e) => SExpr::List(vec![
                SExpr::Atom("ExprStmt".bright_blue().to_string()),
                e.to_sexpr(),
            ]),
            Assign { target, type_name, value } => {
                let mut vec = vec![
                    SExpr::Atom("Assign".bright_blue().to_string()),
                    target.to_sexpr(),
                ];
                vec.push(type_name.to_sexpr());
                vec.push(value.to_sexpr());
                SExpr::List(vec)
            }
            Tell { target, value } => SExpr::List(vec![
                SExpr::Atom("Tell".bright_blue().to_string()),
                target.to_sexpr(),
                value.to_sexpr(),
            ]),
            Handler(handler_arc) => SExpr::List(vec![
                SExpr::Atom("Handler".bright_blue().to_string()),
                handler_arc.to_sexpr(),
            ]),
            Expand { target } => SExpr::List(vec![
                SExpr::Atom("Expand".bright_blue().to_string()),
                target.to_sexpr(),
            ]),
            InvalidBlock => SExpr::Atom("InvalidBlock".bright_blue().to_string()),
        }
    }
}

impl ToSExpr for Handler {
    fn to_sexpr(&self) -> SExpr {
        SExpr::List(vec![
            SExpr::Atom("HandlerStruct".bright_blue().to_string()),
            self.pattern.to_sexpr(),
            self.expr.to_sexpr(),
        ])
    }
}

impl ToSExpr for Block {
    fn to_sexpr(&self) -> SExpr {
        let mut vec = vec![SExpr::Atom("Block".bright_blue().to_string())];
        for stmt in &self.0 {
            vec.push(stmt.to_sexpr());
        }
        SExpr::List(vec)
    }
}

impl ToSExpr for AssignmentTarget {
    fn to_sexpr(&self) -> SExpr {
        use AssignmentTarget::*;
        match self {
            Variable(s) => SExpr::List(vec![
                SExpr::Atom("Variable".bright_blue().to_string()),
                s.to_sexpr(),
            ]),
            Slice { target, index } => SExpr::List(vec![
                SExpr::Atom("Slice".bright_blue().to_string()),
                target.to_sexpr(),
                index.to_sexpr(),
            ]),
            List { elements } => {
                let mut vec = vec![SExpr::Atom("AssignmentList".bright_blue().to_string())];
                for elem in elements {
                    vec.push(elem.to_sexpr());
                }
                SExpr::List(vec)
            }
        }
    }
}

impl ToSExpr for Pattern {
    fn to_sexpr(&self) -> SExpr {
        use Pattern::*;
        match self {
            ValueMatch(v) => SExpr::List(vec![
                SExpr::Atom("ValueMatch".bright_blue().to_string()),
                v.to_sexpr(),
            ]),
            VariableCapture(s) => SExpr::List(vec![
                SExpr::Atom("VariableCapture".bright_blue().to_string()),
                s.to_sexpr(),
            ]),
            BlockCapture(s) => SExpr::List(vec![
                SExpr::Atom("BlockCapture".bright_blue().to_string()),
                s.to_sexpr(),
            ]),
            PredicateCapture(sp_pred) => SExpr::List(vec![
                SExpr::Atom("PredicateCapture".bright_blue().to_string()),
                sp_pred.to_sexpr(),
            ]),
            List(patterns) => {
                let mut vec = vec![SExpr::Atom("PatternList".bright_blue().to_string())];
                for p in patterns {
                    vec.push(p.to_sexpr());
                }
                SExpr::List(vec)
            }
        }
    }
}

impl ToSExpr for Predicate {
    fn to_sexpr(&self) -> SExpr {
        use Predicate::*;
        match self {
            Value(v) => SExpr::List(vec![
                SExpr::Atom("PredicateValue".bright_blue().to_string()),
                v.to_sexpr(),
            ]),
            Variable(s) => SExpr::List(vec![
                SExpr::Atom("PredicateVariable".bright_blue().to_string()),
                s.to_sexpr(),
            ]),
            BinaryExpr { lhs, op, rhs } => SExpr::List(vec![
                SExpr::Atom("PredicateBinary".bright_blue().to_string()),
                lhs.to_sexpr(),
                op.to_sexpr(),
                rhs.to_sexpr(),
            ]),
        }
    }
}

impl ToSExpr for Type {
    fn to_sexpr(&self) -> SExpr {
        let type_str = match self {
            Type::Null => "Null",
            Type::Int => "Int",
            Type::Float => "Float",
            Type::String => "String",
            Type::Boolean => "Boolean",
            Type::List => "List",
            Type::Channel => "Channel",
        };
        SExpr::Atom(type_str.bright_green().to_string())
    }
}
