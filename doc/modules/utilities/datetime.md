## Datetime

The datetime module provides for converting from string to an internal datetime representation and back, and some calculations on the internal format. A couple things are unusual about this module. One: it allows different amounts of precision corresponding to what's in the data. More specifically, the "baseunit" can range from year to second. Two: it picks up on cues in date strings to figure out the format, so you don't have to tell it much.

Thus, a newly created datetime object is "naive"; it modifies itself based on the first date string it reads to reflect the format it finds. (You must tell it whether to expect the month or day first with formats like nn/nn/nnnn.) Thereafter,
it expects the same format. Obviously, there are limits on the flexibility of reading formats. The accepted date field separators are / and -. The week and month names are only in English. In - and / separated formats, a 2-digit year field is accepted and treated as being in the 21st century. In the rare case that you wish to talk about the year 50, you can write it as 0050. (But toString always writes years without zero-padding and does not subtract the
2000 to get 2-digit outputs.) The only accepted time field separator is :. A time can have 2 or 3 such fields corresponding to either hh:mm or hh:mm:ss; there is no provision for fractional seconds. Single digit numbers are accepted (but never produced) in this context. The AM and PM indicators are not accepted or produced &mdash; 24 hour times only.

For converting internal datetimes to strings, you can change various internal settings (baseunit, firstcode, mn1, sep, etc)to get different styles. Make changes to baseunit with the provided setter method, since in certain cases it affects the internal representation.

The provided datetimes type extends datetime to work with lists of dates, all in the same format. There are a few methods: one to append more dates to the list; one to get statistics like earliest & latest date; whether the sequence of dates is monotone (in either direction) and whether there is a constant interval between all the dates. (This will be the case for regularly sampled data, and when it's the case, you can compress the list of dates to two items.)

This code generally assumes integer is a 64b type. However, if you represent only dates (no times) any reasonable date will fit into a 32b integer. And the code should "just work" (although this is untested as I write this comment). This module does not handle timezones, which I view as being about something closer to "spacetime" than just time. It is possible to read date strings that include weekday information, but this information is discarded; there is currently no facility to print weekdays. A reasonable way to think about the range of times correctly handled, and the scheme for representing them, is past times back to year 0, assuming the Gregorian calendar (which of course, was not used until the 16th century) all the way back. And future times into the indefinite future, trillions of years assuming you've got 64b integers.

The mn1 slot controls whether `dd/dd/dd` is interpd as `mm/dd/yy` (mn1 = true) or `dd/mm/yy`.

There are some tricky cases about recognizing formats. In particular, a date string consisting of only a year is ambiguous with integers. We adopt the following compromise. If the string consists of 4 digits, it's treated as a year-only date,
else rejected.

#### datetime: type

    [datetime: mn1]
where `mn1:boolean` is true
    val datetime = extend tuple(...) where {
        method setBaseunit = \mod(newunit: label) -> nothing {...}
    }
welrkj

    [datetimes: 

    val datetimes = extend tuple(...) where {
        method setBaseunit = \mod(newunit: label) -> nothing {...}
        method append = \mod(strgs: list(string)) -> nothing {...}
        method calcStats = \mod() {...}
    }
welrkj

    val dateFields = tuple(year, month, day, hour, minute, second: integer)
This is used to drive a "format it your way" version of datetime.toString.