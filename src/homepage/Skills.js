import React, { useState } from "react"
import handleViewport from 'react-in-viewport';
import {Progress} from 'reactstrap';

const NO_ANIM = 0;
const ANIMATING = 1;
const ANIM_COMPLETE = 2;



class AnimatedSkillBarBlock extends React.Component {

    constructor(props) {
        super(props);
        const {value, alpha, inViewport, animPeriod} = this.props;
        this.state = {
            displayed: 0,
            value: value,
        }
    }

    render() {
        const {value, displayed} = this.state;
        if (this.props.inViewport && value != displayed) {
            this.setState({displayed: value});
        }
        return <Progress value={displayed}/>
    }
}

const AnimatedSkillBar = handleViewport(AnimatedSkillBarBlock, { rootMargin: '-1.0px' });

export {AnimatedSkillBar};