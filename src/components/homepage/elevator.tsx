import React from "react"
import { Container, Col, Row } from "reactstrap"
import styles from "./elevator.module.scss"

export default function ElevatorSection() {
  return (
    <Container tag="section">
      <div className={styles.elevatorBlock}>
        <p>
          The first program I ever wrote was Hello World in Java back in 6th
          grade, when I wanted to make myself a Minecraft mod. With no one to
          teach me, I taught myself, mostly through brute-force googling. At the
          end of all that, I added a few blocks to the game and honestly not
          much else. But I didn't care, I felt like it was a great achievement.
        </p>
        <p>
          Since then, I've explored and built even more interesting projects:
          games in C#, web servers in JavaScript, Android apps in Kotlin, robots
          in C++, neural networks in Python, computer algebra systems in
          Haskell, and so much more. It feels satisfying when I invent a complex
          digital machine that cleverly solves a challenging problem.
        </p>
        <p>
          I've carried this passion for solving problems using computers to Cal
          Poly, where I'm learning about more topics, like computer
          architecture, systems programming, and theory of computation.
          Additionally, I'm working at internships to apply my education in
          practice. At FabTime, I designed a REST API for end users to interface
          with the server, and I was also able to troubleshoot the most
          complicated bugs in the code.
        </p>
        <p>
          I am always learning that there are always new things to learn. When
          I'm not coding, you can probably catch me cooking vegetarian dishes,
          playing the Chinese Erhu, or skateboarding.
        </p>
      </div>
    </Container>
  )
}
