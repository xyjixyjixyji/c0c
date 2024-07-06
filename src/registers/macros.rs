use crate::registers::reg::X86_64Register;

// impl to_64bit(), to_32bit(), to_16bit(), to_8bit() for X86_64Register
macro_rules! decl_to_width {
  ($name:ident, $width:expr) => {
    impl X86_64Register {
      #[allow(dead_code)]
      pub fn $name(&self) -> Self {
        match self {
          X86_64Register::RAX(_) => X86_64Register::RAX($width),
          X86_64Register::RBX(_) => X86_64Register::RBX($width),
          X86_64Register::RCX(_) => X86_64Register::RCX($width),
          X86_64Register::RDX(_) => X86_64Register::RDX($width),
          X86_64Register::RSI(_) => X86_64Register::RSI($width),
          X86_64Register::RDI(_) => X86_64Register::RDI($width),
          X86_64Register::RBP(_) => X86_64Register::RBP($width),
          X86_64Register::RSP(_) => X86_64Register::RSP($width),
          X86_64Register::R8(_) => X86_64Register::R8($width),
          X86_64Register::R9(_) => X86_64Register::R9($width),
          X86_64Register::R10(_) => X86_64Register::R10($width),
          X86_64Register::R11(_) => X86_64Register::R11($width),
          X86_64Register::R12(_) => X86_64Register::R12($width),
          X86_64Register::R13(_) => X86_64Register::R13($width),
          X86_64Register::R14(_) => X86_64Register::R14($width),
          X86_64Register::R15(_) => X86_64Register::R15($width),
        }
      }
    }
  };
}

// impl is_64bit(), is_32bit(), is_16bit(), is_8bit() for X86_64Register
macro_rules! decl_is_width {
  ($name:ident, $width:expr) => {
    impl X86_64Register {
      #[allow(dead_code)]
      pub const fn $name(&self) -> bool {
        self.width() == $width
      }
    }
  };
}

decl_is_width!(is_64bit, 64);
decl_is_width!(is_32bit, 32);
decl_is_width!(is_16bit, 16);
decl_is_width!(is_8bit, 8);

decl_to_width!(as_64bit, 64);
decl_to_width!(as_32bit, 32);
decl_to_width!(as_16bit, 16);
decl_to_width!(as_8bit, 8);
