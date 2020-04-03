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
    id: "misc-skills",
    name: "Miscellaneous",
    color: "#e0c91f",
    children: [
      {
        name: "Pure Functional Programming",
        id: "pure-fp",
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
        name: "Control Systems",
        id: "control-systems",
      },
      {
        name: "Graphic Design",
        id: "graphic",
      },
      {
        name: "3D Printing",
        id: "3d-printing",
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
        name: "PCB Design",
        id: "pcb",
      },
      {
        name: "Circuit Design",
        id: "circuit",
      },
    ],
  },
];
