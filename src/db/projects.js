import {
  imgSegwayBot,
  imgHairNet,
  imgCollisionZone,
  imgTanksberry,
  imgHasCAS,
} from "../assets";

class Project {
  constructor({
    id,
    title,
    descShort,
    date,
    url = null,
    status,
    skills,
    source = null,
    img = null,
  }) {
    this.id = id;
    this.title = title;
    this.desc = descShort;
    this.date = date;
    this.url = url;
    this.status = status;
    this.skills = skills;
    this.source = source;
    this.img = img;
  }
}

const projects = [
  {
    title: "Personal Website",
    id: "personal-website",
    desc: "A website that links to itself",
    date: [new Date(2020, 3)],
    url: "https://plenglin.github.io",
    status: "wip",
    skills: [
      "react",
      "ts",
      "js",
      "css",
      "bootstrap",
      "sass",
      "html",
      "graphic",
    ],
    source: "https://github.com/Plenglin/plenglin.github.io",
    img: null,
  },
  {
    title: "HairNet",
    id: "hair-net",
    desc: "A neural network that detects a person's face and hair",
    date: [new Date(2019, 5), new Date(2019, 8)],
    status: "complete",
    skills: ["tensorflow", "image-processing", "py", "data", "anaconda"],
    source: "https://github.com/Plenglin/hair-net",
    img: imgHairNet,
  },
  {
    title: "Collision Zone",
    id: "collision-zone",
    status: "complete",
    desc: "A IO-style game about crashing cars into each other",
    date: [new Date(2019, 5), new Date(2019, 8)],
    skills: ["c++", "websockets", "ts", "js", "css", "html", "game"],
    url: "http://collision.zone",
    img: imgCollisionZone,
  },
  {
    title: "Mini Segway Bot",
    id: "segway-bot",
    status: "complete",
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
      "me",
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
  {
    title: "HasCAS",
    id: "hascas",
    status: "wip",
    desc: "Haskell Computer Algebra System",
    date: [new Date(2020, 1)],
    skills: ["hs", "ast"],
    source: "https://github.com/Plenglin/HasCAS",
    img: imgHasCAS,
  },
  {
    id: "quest-of-con",
    title: "Quest of Con",
    status: null,
    desc: "Turn-based sci-fi strategy",
    date: [new Date(2017, 9), new Date(2018, 2)],
    skills: ["libgdx", "kt", "terrain-gen", "ux-design"],
    source: "https://github.com/Plenglin/quest-of-con",
  },
  {
    id: "space-admiral",
    title: "Space Admiral",
    status: null,
    desc: "Real-Time Space Opera Strategy",
    date: [new Date(2019, 1), new Date(2019, 5)],
    skills: ["libgdx", "java", "kd-tree"],
    source: "https://github.com/Plenglin/space-admiral",
  },
  {
    id: "goggle-epoxy",
    title: "Goggle Epoxy",
    status: null,
    desc:
      "Google Glass, but cheaper, and engineered worse, and unfortunately, non-functional",
    date: [new Date(2018, 6), new Date(2018, 8)],
    skills: [
      "sensor-fusion",
      "java",
      "raspberry-pi",
      "ar",
      "me",
      "3d-printing",
      "ux-design",
      "iot",
      "wearable",
      "optics",
      "resin-casting",
    ],
    source: "https://github.com/Plenglin/goggle-epoxy",
  },
  {
    id: "tanksberry-pi",
    title: "Tanksberry Pi",
    status: "complete",
    desc: "A tank with a fully-3D printed self-loading BB gun",
    date: [new Date(2017, 5), new Date(2017, 7)],
    skills: [
      "py",
      "circuit",
      "me",
      "ee",
      "raspberry-pi",
      "stepper",
      "3d-printing",
      "html",
      "bootstrap",
    ],
    img: imgTanksberry,
    source: "https://github.com/Plenglin/tanksberry-pi",
  },
];

export default projects;
