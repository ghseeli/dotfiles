add2path() {
    export PATH="$1:$PATH"
}

# Stock
add2path /usr/local/sbin:/usr/textbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin

# Heroku
add2path /usr/local/heroku/bin

# TeX (OSX)
add2path /Library/TeX/texbin

# Python (OSX)
add2path /Library/Frameworks/Python.framework/Versions/2.7/bin

# Sage (OSX)
add2path /Applications/sage

# MySQL
add2path /usr/local/mysql/bin
