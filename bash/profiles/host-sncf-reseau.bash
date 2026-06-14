source "$PJB_BASH_RC_ROOT/bash/profiles/site-user.bash"
source "$PJB_BASH_RC_ROOT/bash/profiles/site-sncf-reseau.bash"

export TERM=dumb
pwsh -commandwithargs chcp 850 2>/dev/null || powershell -commandwithargs chcp 850 2>/dev/null

# export PATH="${PATH}:/c/Program Files/Git/bin/"
# export PATH="${PATH}:/c/Program Files/Git/usr/bin/"
# export PATH="${PATH}:/c/Program Files/make-4.4.1-with-guile-w32/bin/"
# export PATH="${PATH}:/c/Program Files/binutils-2.46-w32/bin/"
# export PATH="${PATH}:/c/Program Files/findutils-4.2.30-5-w64/bin/"
# 
# export PATH="${PATH}:/c/Program Files/Steel Bank Common Lisp/"
# 
# export PATH="${PATH}:/C/Users/PPBN02261/AppData/Local/Microsoft/WinGet/Packages/astral-sh.uv_Microsoft.Winget.Source_8wekyb3d8bbwe"
# 
# export PS1='\n$TITLEPREFIX:$PWD\]\n\[\033[32m\]\u@\h \[\033[35m\]$MSYSTEM\n\[\033[33m\]\w\[\033[36m\]\n`__git_ps1`\[\033[0m\]\n$ '

export JIRA_USER_EMAIL='ext.pascal.bourguignon@reseau.sncf.fr'
export JIRA_USER_NAME=PPBN02261
#export EPURE_INSTALL_DIR="c:\\EPURE2022-pjb"
export EPURE_INSTALL_DIR="c:\\EPURE2022"
export MSBUILD_PATH="C:\\Program Files\\Microsoft Visual Studio\\18\\Community\\MSBuild\\Current\\Bin\\MSBuild.exe"
# echo '!!! We need to re-source .bashrc-schms-functions after modifying !!!'
# echo '!!! these environment variables                                  !!!'
[ -r "$HOME/.bashrc-schms-functions" ] && source "$HOME/.bashrc-schms-functions"
