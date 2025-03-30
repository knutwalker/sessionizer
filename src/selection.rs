use std::fmt::Display;

use fuzzy_select::{FuzzySelect, Select};

use crate::{Entry, Result};

pub struct Selection {
    pub entries: Vec<Entry>,
    pub query: Option<String>,
    pub color: bool,
}

pub fn prompt_user(selection: Selection) -> Result<Option<Entry>> {
    Ok(FuzzySelect::new()
        .with_prompt("Select a session or project: >")
        .set_query::<String>(selection.query)
        .set_color(selection.color)
        .with_options(selection.entries)
        .with_select1()
        .select_opt()?)
}

impl Select for Entry {
    fn search_content(&self) -> &str {
        self.search_content()
    }

    fn render_before_content(&self) -> Option<impl Display + '_> {
        if let Self::Project(project) = self {
            Some(project)
        } else {
            None
        }
    }

    fn render_after_content(&self) -> Option<impl Display + '_> {
        if let Self::Session(tmux) = self {
            Some(tmux)
        } else {
            None
        }
    }
}
