const projects = [
  {
    title: "Personal Website",
    id: "personal-website",
    desc: "This website",
    date: new Date(2020, 3),
    url: "https://plenglin.github.io",
    skills: ["react", "js", "css", "html", "graphic"],
    source: "github:Plenglin/plenglin.github.io",
    img: null, // TODO
  },
  {
    title: "HairNet",
    id: "hair-net",
    desc: "A neural network that detects a person's face and hair",
    date: [new Date(2019, 5), new Date(2019, 8)],
    skills: ["tensorflow", "image-processing", "py", "data", "anaconda"],
    source: "github:Plenglin/hair-net",
    img: null, // TODO
  },
  {
    title: "Collision Zone",
    id: "collision-zone",
    desc: "A IO-style game about crashing cars into each other",
    date: [new Date(2019, 5), new Date(2019, 8)],
    skills: ["c++", "websockets", "ts", "js", "css", "html", "game"],
    url: "http://collision.zone",
    img: null, // TODO
  },
  {
    title: "Mini Segway Bot",
    id: "segway-bot",
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
    source: "github:Plenglin/segway-bot",
    img: null, // TODO
  },
  {
    title: "Inventree",
    id: "inventree",
    desc: "An inventory tracker to keep track of my pile of stuff",
    date: [new Date(2020, 1)],
    skills: ["django", "react", "js"],
  },
];

export default projects;
