use crate::context::JsRuleContext;
use crate::{
    registry::{JsRuleAction, Rule, RuleDiagnostic},
    ActionCategory, RuleCategory,
};
use rome_console::markup;
use rome_diagnostics::Applicability;
use rome_js_factory::make;
use rome_js_syntax::{JsAnyStatement, JsForStatement, JsForStatementFields, T};
use rome_rowan::AstNodeExt;

pub(crate) enum UseWhile {}

impl Rule for UseWhile {
    const NAME: &'static str = "useWhile";
    const CATEGORY: RuleCategory = RuleCategory::Lint;

    type Query = JsForStatement;
    type State = ();

    fn run(ctx: &crate::context::RuleContext<Self>) -> Option<Self::State> {
        let JsForStatementFields {
            for_token: _,
            l_paren_token,
            initializer,
            first_semi_token: _,
            test,
            second_semi_token: _,
            update,
            r_paren_token,
            body,
        } = ctx.query().as_fields();

        if l_paren_token.is_err()
            || initializer.is_some()
            || test.is_none()
            || update.is_some()
            || r_paren_token.is_err()
            || body.is_err()
        {
            None
        } else {
            Some(())
        }
    }

    fn diagnostic(
        ctx: &crate::context::RuleContext<Self>,
        _: &Self::State,
    ) -> Option<RuleDiagnostic> {
        let node = ctx.query();

        // SAFETY: These tokens have been checked for errors in `run` already
        let for_range = node.for_token().unwrap().text_trimmed_range();
        let r_paren_range = node.r_paren_token().unwrap().text_trimmed_range();

        Some(RuleDiagnostic::warning(
            for_range.cover(r_paren_range),
            markup! {
                "Use "<Emphasis>"while"</Emphasis>" loops instead of "<Emphasis>"for"</Emphasis>" loops."
            },
        ))
    }

    fn action(ctx: &crate::context::RuleContext<Self>, _: &Self::State) -> Option<JsRuleAction> {
        let JsForStatementFields {
            for_token: _,
            l_paren_token,
            initializer: _,
            first_semi_token: _,
            test,
            second_semi_token: _,
            update: _,
            r_paren_token,
            body,
        } = ctx.query().as_fields();

        let root = ctx.root().clone().replace_node(
            JsAnyStatement::from(ctx.query().clone()),
            JsAnyStatement::from(make::js_while_statement(
                make::token_decorated_with_space(T![while]),
                l_paren_token.ok()?,
                test?,
                r_paren_token.ok()?,
                body.ok()?,
            )),
        )?;

        Some(JsRuleAction {
            category: ActionCategory::QuickFix,
            applicability: Applicability::MaybeIncorrect,
            message: markup! { "Use a while loop" }.to_owned(),
            root,
        })
    }
}
