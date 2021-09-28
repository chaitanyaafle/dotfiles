




# added by Anaconda3 5.1.0 installer
# export PATH="/anaconda3/bin:$PATH"
# export PATH=${HOME}/.local/bin:${PATH}
export PATH=${HOME}/.local/bin:${PATH}
. ~/.profile
#export PKG_CONFIG_PATH=/opt/local/lib/pkgconfig/


#export poppler_glib_CFLAGS="-I/usr/include/poppler/glib -I/usr/include/cairo"
#export poppler_glib_LIBS="-L/usr/lib -lpoppler -lcairo -lcairo-gobject -lpoppler-glib -lpoppler-cpp -lgobject-2.0 -lglib-2.0"
export PKG_CONFIG_PATH=:/usr/local/Cellar/:/opt/local/lib/pkgconfig:/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig
#export EMACS='t'

##
# Your previous /Users/chaitanya/.bash_profile file was backed up as /Users/chaitanya/.bash_profile.macports-saved_2019-05-27_at_20:53:51
##

# MacPorts Installer addition on 2019-05-27_at_20:53:51: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/chaitanya/opt/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/chaitanya/opt/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/chaitanya/opt/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/chaitanya/opt/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

