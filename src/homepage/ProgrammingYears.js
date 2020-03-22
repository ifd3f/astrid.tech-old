import React from 'react';

const APPX_BEGAN_PROGRAMMING = new Date("2013-02-01");

function ProgrammingYears() {
    const rawYears = (new Date() - APPX_BEGAN_PROGRAMMING) / (1000 * 24 * 3600 * 365);
    if (rawYears % 1.0 < 0.5) {
        return `over ${Math.floor(rawYears)}`;
    } else {
        return `around ${Math.ceil(rawYears)}`
    }
}

export default ProgrammingYears;