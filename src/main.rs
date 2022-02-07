use clap::Parser;
use serde::{
    de::{SeqAccess, Visitor},
    ser::SerializeTuple,
    Deserialize, Deserializer, Serialize, Serializer,
};
use std::{
    io::{stdin, Read},
    path::PathBuf,
    sync::{atomic::{AtomicBool, Ordering}, Arc},
};
use Opcode::*;
use Value::*;

const U15_MAX: u16 = 32767;

#[derive(Parser)]
struct Options {
    #[clap(short, long)]
    load: Option<PathBuf>,
    #[clap(short, long)]
    save: Option<PathBuf>,
}
fn main() -> Result<()> {
    let exit: Arc<AtomicBool> = Arc::new(AtomicBool::new(false));
    {
        let exit = exit.clone();
        ctrlc::set_handler(move || {
            exit.store(true, Ordering::Relaxed);
        }).expect("set ctrl+c handler");
    }
    let options = Options::parse();
    let mut machine = if let Some(path) = &options.load {
        let file = std::fs::File::open(path)?;
        serde_json::from_reader(file)?
    } else {
        let code = include_bytes!("../sources/challenge.bin");

        let mut memory = [0; u16::MAX as usize + 1];
        code.chunks_exact(2)
            .enumerate()
            .for_each(|(i, tuple)| memory[i] = tuple[0] as u16 + ((tuple[1] as u16) << 8));
        let machine = VirtualMachine {
            registers: [0; 8],
            memory,
            stack: Vec::new(),
            pc: 0,
            running: true,
        };
        machine
    };
    while !exit.load(Ordering::Relaxed) && machine.running {
        machine.step()?;
    }
    if let Some(path) = &options.save {
        let file = std::fs::File::create(path)?;
        serde_json::to_writer(file, &machine)?;
    }

    Ok(())
}

type MemoryArray = [Word; u16::MAX as usize + 1];
#[derive(Serialize, Deserialize)]
struct VirtualMachine {
    registers: [Word; 8],
    #[serde(with = "Memory")]
    memory: MemoryArray,
    stack: Vec<Word>,
    pc: u16,
    running: bool,
}

impl VirtualMachine {
    fn step(&mut self) -> Result<()> {
        let opcode = self.opcode()?;
        self.pc += opcode.len();
        match opcode {
            Halt => self.running = false,
            Set(a, b) => self[a] = self.index(b),
            Push(a) => self.stack.push(self.index(a)),
            Pop(a) => self[a] = self.stack.pop().ok_or(Error::PopOnEmptyStack)?,
            Eq(a, b, c) => self[a] = (self.index(b) == self.index(c)) as Word,
            Gt(a, b, c) => self[a] = (self.index(b) > self.index(c)) as Word,
            Jmp(a) => self.pc = self.index(a),
            Jt(a, b) => {
                if self.index(a) != 0 {
                    self.pc = self.index(b)
                }
            }
            Jf(a, b) => {
                if self.index(a) == 0 {
                    self.pc = self.index(b)
                }
            }
            Add(a, b, c) => self[a] = (self.index(b) + self.index(c)) % (U15_MAX + 1),
            Mult(a, b, c) => {
                self[a] =
                    ((self.index(b) as u32 * self.index(c) as u32) % (U15_MAX as u32 + 1)) as u16
            }
            Mod(a, b, c) => self[a] = self.index(b) % self.index(c),
            And(a, b, c) => self[a] = self.index(b) & self.index(c),
            Or(a, b, c) => self[a] = self.index(b) | self.index(c),
            Not(a, b) => self[a] = !(1 << 15) & !self.index(b),
            Rmem(a, b) => self[a] = self.memory[self.index(b) as usize],
            Wmem(a, b) => {
                let a = self.index(a) as usize;
                self.memory[a] = self.index(b);
            }
            Call(a) => {
                self.stack.push(self.pc);
                self.pc = self.index(a);
            }
            Ret => {
                if let Some(ret) = self.stack.pop() {
                    self.pc = ret;
                } else {
                    self.running = false
                }
            }
            Out(a) => print!("{}", char::from(self.index(a) as u8)),
            In(a) => {
                let mut buf = [0];
                if stdin().read_exact(&mut buf).is_ok() {
                    if buf[0] == 3 {
                        return Err(Error::EndOfFile);
                    } else {
                        self[a] = buf[0] as u16;
                    }
                } else {
                    return Err(Error::EndOfFile);
                }
            }
            Noop => {}
        }
        Ok(())
    }

    fn opcode(&self) -> Result<Opcode> {
        let a = self.pc + 1;
        let b = self.pc + 2;
        let c = self.pc + 3;
        Ok(match self.get_memory(self.pc)? {
            0 => Halt,
            1 => Set(self.register(a)?, self.value(b)?),
            2 => Push(self.value(a)?),
            3 => Pop(self.register(a)?),
            4 => Eq(self.register(a)?, self.value(b)?, self.value(c)?),
            5 => Gt(self.register(a)?, self.value(b)?, self.value(c)?),
            6 => Jmp(self.value(a)?),
            7 => Jt(self.value(a)?, self.value(b)?),
            8 => Jf(self.value(a)?, self.value(b)?),
            9 => Add(self.register(a)?, self.value(b)?, self.value(c)?),
            10 => Mult(self.register(a)?, self.value(b)?, self.value(c)?),
            11 => Mod(self.register(a)?, self.value(b)?, self.value(c)?),
            12 => And(self.register(a)?, self.value(b)?, self.value(c)?),
            13 => Or(self.register(a)?, self.value(b)?, self.value(c)?),
            14 => Not(self.register(a)?, self.value(b)?),
            15 => Rmem(self.register(a)?, self.value(b)?),
            16 => Wmem(self.value(a)?, self.value(b)?),
            17 => Call(self.value(a)?),
            18 => Ret,
            19 => Out(self.value(a)?),
            20 => In(self.register(a)?),
            21 => Noop,
            m => return Err(Error::InvalidOpCode(m)),
        })
    }

    fn get_memory(&self, mem: u16) -> Result<Word> {
        Ok(self.memory.get(mem as usize).copied().unwrap_or(0))
    }

    fn register(&self, mem: u16) -> Result<usize> {
        match self.value(mem) {
            Ok(Register(register)) => Ok(register),
            Ok(Immediate(i)) => Err(Error::NotARegister(i)),
            Err(e) => Err(e),
        }
    }

    fn value(&self, mem: u16) -> Result<Value> {
        let value = *self
            .memory
            .get(mem as usize)
            .ok_or(Error::MemoryOutOfBounds(mem))?;
        if value <= 32767 {
            Ok(Immediate(value))
        } else if value <= 32775 {
            Ok(Register((value % 8) as usize))
        } else {
            Err(Error::NotAValue(value))
        }
    }

    fn index(&self, index: Value) -> Word {
        match index {
            Value::Register(index) => self.registers[index],
            Value::Immediate(value) => value,
        }
    }
}

impl std::ops::Index<usize> for VirtualMachine {
    type Output = Word;

    fn index(&self, index: usize) -> &Self::Output {
        &self.registers[index]
    }
}

impl std::ops::IndexMut<usize> for VirtualMachine {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.registers[index]
    }
}

type Word = u16;

enum Opcode {
    Halt,
    Set(Register, Value),
    Push(Value),
    Pop(Register),
    Eq(Register, Value, Value),
    Gt(Register, Value, Value),
    Jmp(Value),
    Jt(Value, Value),
    Jf(Value, Value),
    Add(Register, Value, Value),
    Mult(Register, Value, Value),
    Mod(Register, Value, Value),
    And(Register, Value, Value),
    Or(Register, Value, Value),
    Not(Register, Value),
    Rmem(Register, Value),
    Wmem(Value, Value),
    Call(Value),
    Ret,
    Out(Value),
    In(Register),
    Noop,
}
impl Opcode {
    fn len(&self) -> u16 {
        match self {
            Halt | Ret | Noop => 1,
            Push(_) | Pop(_) | Jmp(_) | Call(_) | Out(_) | In(_) => 2,
            Set(..) | Jt(..) | Jf(..) | Not(..) | Rmem(..) | Wmem(..) => 3,
            Eq(..) | Gt(..) | Add(..) | Mult(..) | Mod(..) | And(..) | Or(..) => 4,
        }
    }
}
type Register = usize;
#[derive(Clone, Copy)]
enum Value {
    Immediate(Word),
    Register(Register),
}

type Result<T> = std::result::Result<T, Error>;

enum Error {
    NotARegister(Word),
    MemoryOutOfBounds(u16),
    NotAValue(Word),
    InvalidOpCode(Word),
    PopOnEmptyStack,
    EndOfFile,
    IOError,
    SerdeError,
}

impl From<serde_json::Error> for Error {
    fn from(_: serde_json::Error) -> Self {
        Self::IOError
    }
}
impl From<std::io::Error> for Error {
    fn from(_: std::io::Error) -> Self {
        Self::SerdeError
    }
}

impl std::error::Error for Error {}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Error::*;
        match self {
            NotARegister(w) => write!(f, "{} is not a register", w),
            MemoryOutOfBounds(u) => write!(f, "{} is an invalid memory address", u),
            NotAValue(w) => write!(f, "{} is neither a register nor a 15 bit value", w),
            InvalidOpCode(w) => write!(f, "{} is not a valid opcode", w),
            PopOnEmptyStack => write!(f, "pop on empty stack"),
            EndOfFile => write!(f, "encountered EOF while trying to read input"),
            IOError => write!(f, "IOError"),
            SerdeError => write!(f, "SerdeError"),
        }
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Display>::fmt(self, f)
    }
}
trait Memory<'de>: Sized {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer;
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>;
}
impl<'de> Memory<'de> for MemoryArray {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_tuple(self.len())?;
        let max = self.iter().rposition(|&x| x != 0).unwrap_or(0);
        for elem in &self[..=max] {
            seq.serialize_element(elem)?;
        }
        seq.end()
    }

    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ArrayVisitor;
        impl<'de> Visitor<'de> for ArrayVisitor {
            type Value = MemoryArray;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "an array of {} elements", Word::MAX as usize + 1)
            }

            fn visit_seq<A>(self, mut seq: A) -> std::result::Result<MemoryArray, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let mut memory = [0; u16::MAX as usize + 1];
                let mut i = 0;
                while let Some(val) = seq.next_element()? {
                    memory[i] = val;
                    i += 1;
                }
                Ok(memory)
            }
        }

        deserializer.deserialize_tuple(Word::MAX as usize + 1, ArrayVisitor)
    }
}
