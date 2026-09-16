import pathlib
import shutil
import subprocess
import tempfile
import tomllib
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[1]


class CodexConfigTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        base = pathlib.Path(self.temp.name)
        self.source = base / 'source'
        self.home = base / 'home'
        self.source.mkdir()
        self.home.mkdir()
        shutil.copytree(ROOT / 'dot_codex', self.source / 'dot_codex',
                        ignore=shutil.ignore_patterns('private_config.local.toml', 'symlink_*', 'hooks*'))
        if (ROOT / '.chezmoitemplates').exists():
            shutil.copytree(ROOT / '.chezmoitemplates', self.source / '.chezmoitemplates')
        shutil.copy(ROOT / '.chezmoiignore', self.source / '.chezmoiignore')
        shutil.copytree(ROOT / '.chezmoidata', self.source / '.chezmoidata')
        self.target = self.home / '.codex/config.toml'
        self.target.parent.mkdir()
        self.command = ['chezmoi', '--source', str(self.source), '--destination', str(self.home),
                        '--config', str(base / 'config.toml'), '--persistent-state', str(base / 'state.db'),
                        '--cache', str(base / 'cache'), '--no-tty']

    def run_chezmoi(self, *args):
        return subprocess.run(self.command + list(args), text=True, capture_output=True, check=True).stdout

    def apply(self):
        self.run_chezmoi('apply', str(self.target))
        return tomllib.loads(self.target.read_text())

    def test_app_state_survives_common_settings_and_repeated_apply(self):
        self.target.write_text('''personality = "friendly"
notify = ["/local/notify", "turn-ended"]
[mcp_servers.node_repl.env]
BROWSER_USE_CODEX_APP_VERSION = "new-version"
[projects."/local/project"]
trust_level = "trusted"
[hooks.state.example]
enabled = false
trusted_hash = "new-hash"
[tui.model_availability_nux]
example = 4
[plugins."local-plugin"]
enabled = true
[desktop]
followUpQueueMode = "queue"
''')
        original = tomllib.loads(self.target.read_text())
        result = self.apply()
        self.assertEqual(result['personality'], 'none')
        for key in ['notify', 'mcp_servers', 'projects', 'hooks', 'desktop']:
            self.assertEqual(result[key], original[key])
        self.assertEqual(result['tui']['model_availability_nux'], original['tui']['model_availability_nux'])
        self.assertEqual(result['plugins']['local-plugin'], {'enabled': True})
        first = self.target.read_text()
        self.apply()
        self.assertEqual(self.target.read_text(), first)
        updated = first + '\n[app_state]\nversion = "next"\n'
        self.target.write_text(updated)
        self.apply()
        self.assertEqual(self.target.read_text(), updated)
        self.assertEqual(self.run_chezmoi('diff', str(self.target)), '')

    def test_local_settings_override_common_values_without_losing_siblings(self):
        (self.source / 'dot_codex/private_config.local.toml').write_text('''personality = "friendly"
[features]
hooks = false
[mcp_servers.custom]
command = "/local/server"
''')
        self.target.write_text('[features]\ncustom = true\n')
        result = self.apply()
        self.assertEqual(result['personality'], 'friendly')
        self.assertFalse(result['features']['hooks'])
        self.assertTrue(result['features']['custom'])
        self.assertEqual(result['mcp_servers']['custom']['command'], '/local/server')
        self.assertNotIn('.codex/config.local.toml', self.run_chezmoi('managed').splitlines())

    def test_fresh_install_creates_private_config(self):
        result = self.apply()
        self.assertEqual(result['default_permissions'], 'workspace-with-dev-tools')
        self.assertEqual(self.target.stat().st_mode & 0o777, 0o600)

    def test_invalid_existing_toml_is_not_overwritten(self):
        self.target.write_text('[broken')
        with self.assertRaises(subprocess.CalledProcessError):
            self.apply()
        self.assertEqual(self.target.read_text(), '[broken')


if __name__ == '__main__':
    unittest.main()
