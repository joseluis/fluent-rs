use core::{fmt, ops::Range};

/// Error containing information about an error encountered by the Fluent Parser.
///
/// Errors in Fluent Parser are non-fatal, and the syntax has been
/// designed to allow for strong recovery.
///
/// In result [`ParserError`] is designed to point at the slice of
/// the input that is most likely to be a complete fragment from after
/// the end of a valid entry, to the start of the next valid entry, with
/// the invalid syntax in the middle.
///
///
/// # Example
///
/// ```
/// use fluent_syntax::parser;
/// use fluent_syntax::ast;
///
/// let ftl = r#"
/// key1 = Value 1
///
/// g@Rb@ge = #2y ds
///
/// key2 = Value 2
///
/// "#;
///
/// let (resource, errors) = parser::parse_runtime(ftl)
///     .expect_err("Resource should contain errors.");
///
/// assert_eq!(
///     errors,
///     vec![
///         parser::ParserError {
///             pos: 18..19,
///             slice: Some(17..35),
///             kind: parser::ErrorKind::ExpectedToken('=')
///         }
///     ]
/// );
///
/// assert_eq!(
///     resource.body[0],
///     ast::Entry::Message(
///         ast::Message {
///             id: ast::Identifier {
///                 name: "key1"
///             },
///             value: Some(ast::Pattern {
///                 elements: vec![
///                     ast::PatternElement::TextElement {
///                         value: "Value 1"
///                     },
///                 ]
///             }),
///             attributes: vec![],
///             comment: None,
///         }
///     ),
/// );
///
/// assert_eq!(
///     resource.body[1],
///     ast::Entry::Junk {
///         content: "g@Rb@ge = #2y ds\n\n"
///     }
/// );
///
/// assert_eq!(
///     resource.body[2],
///     ast::Entry::Message(
///         ast::Message {
///             id: ast::Identifier {
///                 name: "key2"
///             },
///             value: Some(ast::Pattern {
///                 elements: vec![
///                     ast::PatternElement::TextElement {
///                         value: "Value 2"
///                     },
///                 ]
///             }),
///             attributes: vec![],
///             comment: None,
///         }
///     ),
/// );
/// ```
///
/// The information contained in the `ParserError` should allow the tooling
/// to display rich contextual annotations of the error slice, using
/// crates such as `annotate-snippers`.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParserError {
    /// Precise location of where the parser encountered the error.
    pub pos: Range<usize>,
    /// Slice of the input from the end of the last valid entry to the beginning
    /// of the next valid entry with the invalid syntax in the middle.
    pub slice: Option<Range<usize>>,
    /// The type of the error that the parser encountered.
    pub kind: ErrorKind,
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl core::error::Error for ParserError {}

macro_rules! error {
    ($kind:expr, $start:expr) => {{
        Err(ParserError {
            pos: $start..$start + 1,
            slice: None,
            kind: $kind,
        })
    }};
    ($kind:expr, $start:expr, $end:expr) => {{
        Err(ParserError {
            pos: $start..$end,
            slice: None,
            kind: $kind,
        })
    }};
}

/// Kind of an error associated with the [`ParserError`].
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ErrorKind {
    ExpectedToken(char),
    ExpectedCharRange { range: String },
    ExpectedMessageField { entry_id: String },
    ExpectedTermField { entry_id: String },
    ForbiddenCallee,
    MissingDefaultVariant,
    MissingValue,
    MultipleDefaultVariants,
    MessageReferenceAsSelector,
    TermReferenceAsSelector,
    MessageAttributeAsSelector,
    TermAttributeAsPlaceable,
    UnterminatedStringLiteral,
    PositionalArgumentFollowsNamed,
    DuplicatedNamedArgument(String),
    UnknownEscapeSequence(String),
    InvalidUnicodeEscapeSequence(String),
    UnbalancedClosingBrace,
    ExpectedInlineExpression,
    ExpectedSimpleExpressionAsSelector,
    ExpectedLiteral,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorKind as K;
        match self {
            K::ExpectedToken(ch) => write!(f, "Expected a token starting with \"{}\"", ch),
            K::ExpectedCharRange { range } => write!(f, "Expected one of \"{}\"", range),
            K::ExpectedMessageField { entry_id } => {
                write!(f, "Expected a message field for \"{}\"", entry_id)
            }
            K::ExpectedTermField { entry_id } => {
                write!(f, "Expected a term field for \"{}\"", entry_id)
            }
            K::ForbiddenCallee => write!(f, "Callee is not allowed here"),
            K::MissingDefaultVariant => {
                write!(f, "The select expression must have a default variant")
            }
            K::MissingValue => write!(f, "Expected a value"),
            K::MultipleDefaultVariants => {
                write!(f, "A select expression can only have one default variant")
            }
            K::MessageReferenceAsSelector => {
                write!(f, "Message references can't be used as a selector")
            }
            K::TermReferenceAsSelector => {
                write!(f, "Term references can't be used as a selector")
            }
            K::MessageAttributeAsSelector => {
                write!(f, "Message attributes can't be used as a selector")
            }
            K::TermAttributeAsPlaceable => {
                write!(f, "Term attributes can't be used as a selector")
            }
            K::UnterminatedStringLiteral => write!(f, "Unterminated string literal"),
            K::PositionalArgumentFollowsNamed => {
                write!(f, "Positional arguments must come before named arguments")
            }
            K::DuplicatedNamedArgument(arg) => {
                write!(f, "The \"{}\" argument appears twice", arg)
            }
            K::UnknownEscapeSequence(seq) => {
                write!(f, "Unknown escape sequence: \"{}\"", seq)
            }
            K::InvalidUnicodeEscapeSequence(seq) => {
                write!(f, "Invalid unicode escape sequence: \"{}\"", seq)
            }
            K::UnbalancedClosingBrace => write!(f, "Unbalanced closing brace"),
            K::ExpectedInlineExpression => write!(f, "Expected an inline expression"),
            K::ExpectedSimpleExpressionAsSelector => {
                write!(f, "Expected a simple expression as selector")
            }
            K::ExpectedLiteral => write!(f, "Expected a string or number literal"),
        }
    }
}
