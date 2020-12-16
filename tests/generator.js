const { rrulestr } = require("rrule");
const { DateTime } = require("luxon");

const padLeft = (
    item,
    targetLength,
    padString = ' '
) => {
    const item_ = String(item);
    if (item_.length >= targetLength) return item_;
    return padLeft(padString + item_, targetLength, padString);
}

const toDTSTART = (posix, mTimezone) => {
    const timezone = mTimezone
        ? mTimezone
        : // All-Day events have no timezone. Default to browser's timezone.
        Intl.DateTimeFormat().resolvedOptions().timeZone;

    const date = DateTime.fromMillis(posix).setZone(timezone);

    // iCalendar style ISO8601
    const startTime = [
        padLeft(date.year, 4, '0'),
        padLeft(date.month, 2, '0'),
        padLeft(date.day, 2, '0'),
        'T',
        padLeft(date.hour, 2, '0'),
        padLeft(date.minute, 2, '0'),
        padLeft(date.second, 2, '0'),
    ].join('');

    return 'DTSTART;TZID=' + timezone + ':' + startTime + ';';
};

const toUTC = posix =>
    DateTime.fromMillis(posix).setZone('utc', { keepLocalTime: true }).toJSDate();

const all = (rrules, mTimeZone) =>
    rrulestr(rrules)
        .all()
        .map((date) => {
            if (mTimeZone !== "UTC") {
                return DateTime.fromJSDate(date)
                    .toUTC()
                    .setZone("local", { keepLocalTime: true })
                    .toMillis();
            } else {
                return date.getTime();
            }
        });

const between = (rrules, mTimeZone) => (windowStart, windowEnd) => {
    // Yes, this `setZone` and `toUTC` business looks confusing.
    // rrule.js has some unusual API decisions for handling timezones.
    // See https://github.com/jakubroztocil/rrule#important-use-utc-dates
    const [start, end] = [
        toUTC(windowStart),
        toUTC(windowEnd),
    ];

    return rrulestr(rrules)
        .between(start, end, true)
        .map(date => {
            if (mTimeZone !== 'UTC') {
                return DateTime.fromJSDate(date)
                    .toUTC()
                    .setZone('local', { keepLocalTime: true })
                    .toMillis();
            } else {
                return date.getTime();
            }
        });
}


module.exports = {all, toDTSTART, between};