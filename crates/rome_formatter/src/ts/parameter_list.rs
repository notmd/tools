use crate::{
    empty_element, format_elements, group_elements, if_group_breaks, join_elements, soft_indent,
    soft_line_break_or_space, space_token, token, FormatElement, FormatResult, Formatter,
    ToFormatElement,
};
use rslint_parser::ast::{JsAnyParameter, JsParameter, JsParameters, JsRestParameter};

impl ToFormatElement for JsParameters {
    fn to_format_element(&self, formatter: &Formatter) -> FormatResult<FormatElement> {
        let param_tokens = formatter.format_separated(self.items())?;

        Ok(group_elements(format_elements![
            formatter.format_token(&self.l_paren_token()?)?,
            soft_indent(format_elements![
                join_elements(soft_line_break_or_space(), param_tokens),
                if_group_breaks(token(",")),
            ]),
            formatter.format_token(&self.r_paren_token()?)?
        ]))
    }
}

impl ToFormatElement for JsAnyParameter {
    fn to_format_element(&self, formatter: &Formatter) -> FormatResult<FormatElement> {
        match self {
            JsAnyParameter::JsParameter(parameter) => parameter.to_format_element(formatter),
            JsAnyParameter::JsUnknownParameter(_) => todo!(),
            JsAnyParameter::JsRestParameter(binding) => binding.to_format_element(formatter),
        }
    }
}

impl ToFormatElement for JsRestParameter {
    fn to_format_element(&self, formatter: &Formatter) -> FormatResult<FormatElement> {
        Ok(format_elements![
            formatter.format_token(&self.dotdotdot_token()?)?,
            formatter.format_node(self.binding()?)?
        ])
    }
}

impl ToFormatElement for JsParameter {
    fn to_format_element(&self, formatter: &Formatter) -> FormatResult<FormatElement> {
        let initializer = if let Some(initializer) = self.initializer() {
            format_elements![space_token(), formatter.format_node(initializer)?]
        } else {
            empty_element()
        };

        Ok(format_elements![
            formatter.format_node(self.binding()?)?,
            initializer
        ])
    }
}
