use zed_extension_api::{self as zed, LanguageServerId, Result};

struct Korektor;

impl zed::Extension for Korektor {
    fn new() -> Self {
        Korektor
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let command = worktree.which("korektor").ok_or_else(|| {
            "korektor isn't on your PATH. install it from Terra with `sudo dnf install korektor`"
                .to_string()
        })?;
        Ok(zed::Command {
            command,
            args: vec!["lsp".to_string()],
            env: worktree.shell_env(),
        })
    }
}

zed::register_extension!(Korektor);
