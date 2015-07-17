#!/usr/bin/env python

import pytz
import datetime


CLOCKS = (
    ('Sask', 'Canada/Saskatchewan'),
    ('Mountain', 'Canada/Mountain'),
    ('Eastern', 'Canada/Eastern'),
    ('Atlantic', 'Canada/Atlantic'),
    ('Yukon', 'Canada/Yukon'),
    ('UTC', 'UTC')
)

FMT = '%-16s%s'
DASHLEN = 16 + 5


def main():
    central = pytz.timezone('Canada/Central')
    now = central.localize(datetime.datetime.now())

    print FMT % ("Timezone", "Time")
    print "=" * DASHLEN

    for desc, tzname in CLOCKS:
        tz = pytz.timezone(tzname)
        loc_now = now.astimezone(tz)

        print FMT % (desc, loc_now.strftime("%H:%M"))

if __name__ == '__main__':
    main()
