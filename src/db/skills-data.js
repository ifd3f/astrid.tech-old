class Skill {
  constructor({ id, level = null, name }) {
    this.id = id;
    this.level = level;
    this.name = name;
  }
}

export default [
  {
    id: "language",
    name: "Language",
    color: "#f7b40c",
    children: [
      {
        name: "Java",
        level: 80,
        id: "java",
      },
      {
        name: "C++",
        level: 70,
        id: "c++",
      },
      {
        name: "Kotlin",
        level: 80,
        id: "kt",
        implied: ["java"],
      },
      {
        name: "Python",
        level: 90,
        id: "py",
      },
      {
        name: "C#",
        level: 70,
        id: "c#",
      },
      {
        name: "C",
        id: "c",
      },
      {
        name: "VBScript",
        level: 70,
        id: "vbs",
      },
      {
        name: "Haskell",
        id: "hs",
        implied: ["pure-fp"],
      },
      {
        name: "Shell Scripting",
        id: "sh",
      },
      {
        name: "JavaScript",
        level: 90,
        id: "js",
      },
      {
        name: "TypeScript",
        level: 65,
        id: "ts",
      },
    ],
  },
  {
    id: "concepts",
    name: "Concepts",
    color: "#ff0066",
    children: [
      {
        name: "Optics",
        id: "optics",
      },
      {
        name: "Abstract Syntax Trees",
        id: "ast",
      },
      {
        name: "Control Systems",
        id: "control-systems",
      },
      {
        name: "k-d Trees",
        id: "kd-tree",
      },
    ],
  },
  {
    id: "misc-skills",
    name: "Miscellaneous",
    color: "#e0c91f",
    children: [
      {
        name: "Augmented Reality",
        id: "ar",
      },
      {
        name: "Pure Functional Programming",
        id: "pure-fp",
      },
      {
        name: "UX Design",
        id: "ux-design",
      },
      {
        name: "Robotics",
        id: "robotics",
      },
      {
        name: "Websockets",
        id: "websockets",
      },
      {
        name: "Graphic Design",
        id: "graphic",
      },
      {
        name: "3D Printing",
        id: "3d-printing",
      },
      {
        name: "Resin Casting",
        id: "resin-casting",
      },
      {
        name: "Mechanical Engineering",
        id: "me",
      },
    ],
  },
  {
    id: "frontend",
    name: "Frontend",
    color: "#31b8f7",
    children: [
      {
        name: "CSS",
        level: 70,
        id: "css",
      },
      {
        name: "Sass",
        id: "sass",
      },
      {
        name: "HTML",
        level: 80,
        id: "html",
      },
      {
        name: "SystemVerilog",
        level: 35,
        id: "sv",
        impliedShown: ["fpgas"],
      },
      {
        name: "Bootstrap",
        level: 80,
        id: "bootstrap",
      },
      {
        name: "React",
        level: 60,
        id: "react",
      },
      {
        name: "jQuery",
        level: 80,
        id: "jquery",
      },
    ],
  },
  {
    id: "backend",
    name: "Backend",
    color: "#4a7aad",
    children: [
      {
        name: "Django",
        implied: ["py"],
        id: "django",
      },
      {
        name: "ASP Classic",
        implied: ["vbs"],
        id: "asp-classic",
      },
      {
        name: "ASP.NET Core",
        implied: ["c#"],
        id: "asp-net-core",
      },
      {
        name: "Node.js",
        level: 50,
        id: "nodejs",
        implied: ["js"],
      },
    ],
  },
  {
    id: "db",
    name: "Database",
    color: "#525569",
    children: [
      { name: "SQL Server", id: "sql-server" },
      { name: "PostgreSQL", id: "postgres" },
      { name: "MySQL", id: "mysql" },
    ],
  },
  {
    id: "game",
    name: "Game Dev",
    color: "#57d608",
    children: [
      {
        name: "LibGDX",
        level: 70,
        id: "libgdx",
      },
      {
        name: "Unity",
        level: 70,
        id: "unity",
      },
      {
        name: "Terrain Generation",
        id: "terrain-gen",
      },
    ],
  },
  {
    id: "data",
    name: "Data Science",
    color: "#e631f7",
    children: [
      {
        name: "Image Processing",
        id: "image-processing",
      },
      {
        name: "Anaconda",
        level: 30,
        implied: ["py"],
        id: "anaconda",
      },
      {
        name: "TensorFlow",
        level: 30,
        implied: ["py"],
        id: "tensorflow",
      },
      {
        name: "OpenCV",
        level: 50,
        impliedShown: ["image-processing"],
        id: "opencv",
      },
      {
        name: "Pandas",
        implied: ["python"],
        id: "pandas",
      },
      {
        name: "Numpy",
        implied: ["python"],
        id: "numpy",
      },
    ],
  },
  {
    id: "ee",
    name: "Electrical Engineering",
    color: "#05befc",
    children: [
      {
        name: "Microcontrollers",
        impliedShown: ["circuit"],
        id: "microcontroller",
      },
      {
        name: "Internet of Things",
        id: "iot",
      },
      {
        name: "Wearables",
        id: "wearable",
      },
      {
        name: "FPGAs",
        id: "fpga",
      },
      {
        name: "Arduino",
        level: 70,
        impliedShown: ["c++", "microcontroller"],
        id: "ino",
      },
      {
        name: "Raspberry Pi",
        id: "raspberry-pi",
      },
      {
        name: "PCB Design",
        id: "pcb",
      },
      {
        name: "Circuit Design",
        id: "circuit",
      },
      {
        name: "Sensor Fusion",
        id: "sensor-fusion",
      },
      {
        name: "Stepper Motors",
        id: "stepper",
      },
    ],
  },
];
