# elm-rrule

iCalendar (RFC 5545) recurring events in Elm    

#### Unsupported RRULE properties:

```
  BYSECOND, BYMINUTE, BYHOUR, BYSETPOS 
  FREQ = SECONDLY, MINUTELY, or HOURLY
```

Reasoning is anything less than a DAILY frequency is not used by Google Calendar or Microsoft Outlook,  
and the time information is baked into DTSTART.

"Floating times" are not yet supported.  
DTSTART, EXDATE, RDATE are expected to specify TZID.


## Test Suite   

[![CircleCI](https://circleci.com/gh/RealKinetic/elm-rrule.svg?style=shield)](https://circleci.com/gh/RealKinetic/elm-rrule)

###### Tests are based on the examples in the iCalendar Spec
<https://tools.ietf.org/html/rfc5545#section-3.8.5.3>

- [x] Daily
- [x] Weekly
- [x] Monthly
- [x] Yearly
