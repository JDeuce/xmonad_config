#!/bin/bash

# Tunables
RESOLUTION=1920
WIDTH=140
HEIGHT=16
DEFAULT_SCREEN=2


# Make a FIFO based on the current PID
FIFO=/tmp/dzen-calendar-$$

# Read the screen from command line arg
SCREEN=${1-$DEFAULT_SCREEN}

# Grab the current date
DAY=$(date +'%d')
CURR_MONTH=$(date +'%m')
CURR_YEAR=$(date +'%Y')
WINTITLE=dzen_popup_calendar_$SCREEN


# Kill any popups that are open
pkill -f $WINTITLE

# If a window was open, then just close this one
# This is so your parent dzen can have a ca that spawns this script,
# and it will act as a toggle
[ $? -eq 0 ] && exit


# Make a FIFO so we can have comms from the dzen window back
# to our main calendar loop
mkfifo $FIFO

# Make sure the FIFO doesn't stay around
trap "rm $FIFO; exit" SIGINT SIGTERM

{
    echo Calendar

    # Just start reading lines from the fifo,
    # outputting a calendar for that month/year
    while read month year;
    do
        next_month=$(date +'%m %Y' -d "${year}${month}01 + 1 month")
        last_month=$(date +'%m %Y' -d "${year}${month}01 - 1 month")

        cal -h $month $year |
            sed \
                -r \
                -e '1 s/^   / < /' `# Add carets to the month` \
                -e '1 s/   $/ > /' `# Then color the carets below` \
                -e "s/( [<|>] )/^ca(1,cmd\1)^fg(\#00FF00)\1^ca()^fg()/g" \
                -e "s#cmd < #echo $last_month#" `# make clicking the caret echo the next month` \
                -e "s#cmd > #echo $next_month#" | \
            {
                # And if we're displaying the current month,
                # we should highlight the current day
                [ $month -eq $CURR_MONTH ] && \
                sed -re "s/(^| )($DAY)( |$)/\1^fg(\#00FF00)\2\3^fg()/" || \
                cat
            }

        /home/jjaques/.xmonad/bin/world_clock.py
    done < $FIFO;

} | \
    dzen2 \
    -xs $SCREEN \
    -x $(expr $RESOLUTION - $WIDTH) \
    -y $HEIGHT \
    -w $WIDTH \
    -l 15 \
    -fg '#ffffff' \
    -sa 'c' \
    -ta 'c' \
    -title-name $WINTITLE \
    -p \
    -e "onstart=uncollapse,exec:echo $CURR_MONTH $CURR_YEAR;button1=exit:0" > $FIFO

rm $FIFO
