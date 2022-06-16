use crate::context::JsRuleContext;
use crate::registry::{Rule, RuleAction, RuleDiagnostic};
use crate::{ActionCategory, LanguageOfRule, RuleCategory};
use rome_console::markup;
use rome_diagnostics::Applicability;
use rome_js_factory::make;
use rome_js_syntax::{
    JsAnyExpression, JsAnyLiteralExpression, JsBinaryExpression, JsSyntaxKind, JsUnaryOperator,
};
use rome_rowan::AstNode;
use rome_rowan::AstNodeExt;
use rome_rowan::SyntaxToken;
pub struct NoCompareNegZeroState {
    operator_kind: &'static str,
    left_need_replaced: bool,
    right_need_replaced: bool,
}
pub(crate) enum NoCompareNegZero {}

impl Rule for NoCompareNegZero {
    const NAME: &'static str = "noCompareNegZero";
    const CATEGORY: RuleCategory = RuleCategory::Lint;

    type Query = JsBinaryExpression;
    type State = NoCompareNegZeroState;

    fn run(ctx: &crate::context::RuleContext<Self>) -> Option<Self::State> {
        let node = ctx.query();

        if !node.is_comparison_operator() {
            return None;
        }

        let op = node.operator_token().ok()?;
        let left = node.left().ok()?;
        let right = node.right().ok()?;
        let is_left_neg_zero = is_neg_zero(&left).unwrap_or(false);
        let is_right_neg_zero = is_neg_zero(&right).unwrap_or(false);
        if is_left_neg_zero || is_right_neg_zero {
            // SAFETY: Because we know those T![>] | T![>=] | T![<] | T![<=] | T![==] | T![===] | T![!=] | T![!==] SyntaxKind will
            // always success in to_string, you could look at our test case `noCompareNegZero.js`
            let operator_kind = op.kind().to_string().unwrap();

            Some(NoCompareNegZeroState {
                operator_kind,
                left_need_replaced: is_left_neg_zero,
                right_need_replaced: is_right_neg_zero,
            })
        } else {
            None
        }
    }

    fn diagnostic(
        ctx: &crate::context::RuleContext<Self>,
        state: &Self::State,
    ) -> Option<RuleDiagnostic> {
        Some(RuleDiagnostic::warning(
            ctx.query().range(),
            markup! {
                "Do not use the "{state.operator_kind}" operator to compare against -0."
            },
        ))
    }
    fn action(
        ctx: &crate::context::RuleContext<Self>,
        state: &Self::State,
    ) -> Option<RuleAction<LanguageOfRule<Self>>> {
        let root = if state.left_need_replaced && state.right_need_replaced {
            let binary = ctx.query().clone().replace_node(
                ctx.query().left().ok()?,
                JsAnyExpression::JsAnyLiteralExpression(
                    JsAnyLiteralExpression::JsNumberLiteralExpression(
                        make::js_number_literal_expression(SyntaxToken::new_detached(
                            JsSyntaxKind::JS_NUMBER_LITERAL,
                            "0",
                            [],
                            [],
                        )),
                    ),
                ),
            )?;
            // extract binary.right() as an extra variable because `binary.replace_node` will move ownership.
            let binary_right = binary.right().ok()?;
            let binary = binary.replace_node(
                binary_right,
                JsAnyExpression::JsAnyLiteralExpression(
                    JsAnyLiteralExpression::JsNumberLiteralExpression(
                        make::js_number_literal_expression(SyntaxToken::new_detached(
                            JsSyntaxKind::JS_NUMBER_LITERAL,
                            "0",
                            [],
                            [],
                        )),
                    ),
                ),
            )?;
            ctx.root()
                .clone()
                .replace_node(ctx.query().clone(), binary)?
        } else if state.left_need_replaced {
            ctx.root().clone().replace_node(
                ctx.query().left().ok()?,
                JsAnyExpression::JsAnyLiteralExpression(
                    JsAnyLiteralExpression::JsNumberLiteralExpression(
                        make::js_number_literal_expression(SyntaxToken::new_detached(
                            JsSyntaxKind::JS_NUMBER_LITERAL,
                            "0",
                            [],
                            [],
                        )),
                    ),
                ),
            )?
        } else if state.right_need_replaced {
            ctx.root().clone().replace_node(
                ctx.query().right().ok()?,
                JsAnyExpression::JsAnyLiteralExpression(
                    JsAnyLiteralExpression::JsNumberLiteralExpression(
                        make::js_number_literal_expression(SyntaxToken::new_detached(
                            JsSyntaxKind::JS_NUMBER_LITERAL,
                            "0",
                            [],
                            [],
                        )),
                    ),
                ),
            )?
        } else {
            ctx.root().clone()
        };

        Some(RuleAction {
            category: ActionCategory::QuickFix,
            applicability: Applicability::Always,
            message: markup! { "Replace -0 with 0" }.to_owned(),
            root,
        })
    }
}

fn is_neg_zero(node: &JsAnyExpression) -> Option<bool> {
    match node {
        JsAnyExpression::JsUnaryExpression(expr) => {
            if !matches!(expr.operator().ok()?, JsUnaryOperator::Minus) {
                return Some(false);
            }
            let argument = expr.argument().ok()?;

            if let JsAnyExpression::JsAnyLiteralExpression(
                JsAnyLiteralExpression::JsNumberLiteralExpression(expr),
            ) = argument
            {
                Some(expr.value_token().ok()?.text_trimmed() == "0")
            } else {
                Some(false)
            }
        }
        _ => Some(false),
    }
}
