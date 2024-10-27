git config -f .gitmodules --remove-section submodule.$1
git rm -r --cached $1
rm -rf $1
rm -rf .git/modules/$1