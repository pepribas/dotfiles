#!/usr/bin/env python
import argparse
import os
import subprocess


gituser_tmpl = """\
[user]
\temail = {}
\tname = {}
"""


symlink_mapping = (
    ('git/gitconfig', '~/.gitconfig'),
    ('git/gitignore', '~/.gitignore'),
    ('bashrc', '~/.bashrc'),
    ('zsh/zlogin', '~/.zlogin'),
    ('zsh/zlogout', '~/.zlogout'),
    ('zsh/prezto', '~/.zprezto'),
    ('zsh/zpreztorc', '~/.zpreztorc'),
    ('zsh/zprofile', '~/.zprofile'),
    ('zsh/custom', '~/.zsh.custom'),
    ('zsh/zshenv', '~/.zshenv'),
    ('zsh/zshrc', '~/.zshrc'),
    ('vim', '~/.vim'),
    ('vim/vimrc', '~/.vimrc'),
    ('vim/gvimrc', '~/.gvimrc'),
)


def run(cmd):
    "Run command as if in shell. Blocking."
    p = subprocess.Popen(cmd, shell=True)
    p.wait()


def gen_git_config(email=None, name=None, force=False):
    gituser = os.path.expanduser('~/.gitconfig.d/user')
    print "Setting up git user"

    if os.path.exists(gituser) and not force:
        print "\tfile exists at path"
        return

    if email is None:
        email = raw_input("git email: ")
    if name is None:
        name = raw_input("git name: ")

    cfg = gituser_tmpl.format(email, name)

    if not os.path.exists(os.path.dirname(gituser)):
        os.makedirs(os.path.dirname(gituser))

    with open(gituser, 'w') as f:
        f.write(cfg)
    print gituser
    print "\tcreated git user config"


def install_symlinks(force=False):
    print("Installing symlinks")
    for path, symlink in symlink_mapping:
        print "{}:".format(symlink)
        file_path = os.path.abspath(os.path.join(os.path.dirname(__file__), path))
        symlink_path = os.path.expanduser(symlink)
        if os.path.islink(symlink_path) and force:
            print "\tremoving existing symlink"
            os.remove(symlink_path)
        elif os.path.isfile(symlink_path) or os.path.islink(symlink_path):
            print "\tfile exists at path"
            continue
        print "\tcreating symlink"
        os.symlink(file_path, symlink_path)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Set up your symlinks!")
    parser.add_argument('-f', '--force',
                        default=False,
                        dest='force',
                        action='store_true',
                        help="if a symlink already exists, overwrite it")
    parser.add_argument('-g', '--skip-git',
                        default=False,
                        dest='skip_git',
                        action='store_true',
                        help="don't setup git user")
    parser.add_argument('-v', '--skip-vim',
                        default=False,
                        dest='skip_vim',
                        action='store_true',
                        help="don't setup vim vundle")
    args = parser.parse_args()

    os.chdir(os.path.abspath(os.path.dirname(__file__)))
    run('git submodule update --init --recursive')

    install_symlinks(force=args.force)

    if not args.skip_vim:
        run('vim +BundleUpdate +qall')

    if not args.skip_git:
        gen_git_config(force=args.force)

