const { rrulestr } = require("rrule");
const { DateTime } = require("luxon");

const generate = (rrules, mTimeZone) =>
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

module.exports = generate;
