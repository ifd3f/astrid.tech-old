import { program } from 'commander'

 
program
  .version('0.1.0')
  .option('-d, --db [path]', 'Database')
  .parse(process.argv)

console.log(program.getOptionValue('db'))
