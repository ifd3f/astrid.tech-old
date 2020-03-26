import React from 'react';

const APPX_BEGAN_PROGRAMMING = new Date("2013-02-01");
const APPX_BEGAN_HARDWARE = new Date("2015-12-15");

export function YearsSince(props) {
    const {date} = props;
    const rawYears = (new Date() - date) / (1000 * 24 * 3600 * 365);
    if (rawYears % 1.0 < 0.5) {
        return `over ${Math.floor(rawYears)}`;
    } else {
        return `around ${Math.ceil(rawYears)}`
    }
}

export function ProgrammingYears(props) {
    return <YearsSince date={APPX_BEGAN_PROGRAMMING}/>
}

export function HardwareYears(props) {
    return <YearsSince date={APPX_BEGAN_HARDWARE}/>
}
