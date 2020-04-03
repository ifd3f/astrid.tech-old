import { imgSegwayBot, imgHairNet, imgCollisionZone } from "../assets";

const projects = [
  {
    title: "Personal Website",
    id: "personal-website",
    desc: "A website that links to itself",
    date: new Date(2020, 3),
    url: "https://plenglin.github.io",
    status: "wip",
    skills: ["react", "js", "css", "html", "graphic"],
    source: "https://github.com/Plenglin/plenglin.github.io",
    img: null,
  },
  {
    title: "HairNet",
    id: "hair-net",
    desc: "A neural network that detects a person's face and hair",
    date: [new Date(2019, 5), new Date(2019, 8)],
    status: "suspended",
    skills: ["tensorflow", "image-processing", "py", "data", "anaconda"],
    source: "https://github.com/Plenglin/hair-net",
    img: imgHairNet,
  },
  {
    title: "Collision Zone",
    id: "collision-zone",
    status: "suspended",
    desc: "A IO-style game about crashing cars into each other",
    date: [new Date(2019, 5), new Date(2019, 8)],
    skills: ["c++", "websockets", "ts", "js", "css", "html", "game"],
    url: "http://collision.zone",
    img: imgCollisionZone,
  },
  {
    title: "Mini Segway Bot",
    id: "segway-bot",
    status: "suspended",
    desc: "A robot that balances on two wheels",
    date: [new Date(2019, 1), new Date(2019, 3)],
    skills: [
      "control-systems",
      "robotics",
      "ino",
      "c++",
      "circuit",
      "pcb",
      "3d-printing",
    ],
    source: "https://github.com/Plenglin/segwaybot",
    img: imgSegwayBot, // TODO
  },
  {
    title: "Inventree",
    id: "inventree",
    status: "wip",
    desc: "An inventory tracker to keep track of my pile of stuff",
    date: [new Date(2020, 1)],
    skills: ["django", "react", "js"],
    source: "https://github.com/Plenglin/unrefined-stockpile",
  },
];

export default projects;
