// SPDX-License-Identifier: BSD-2-Clause-FreeBSD
//
// Copyright (c) 1997, 1998, 1999, 2002 The NetBSD Foundation, Inc.
// Copyright 2013 John-Mark Gurney <jmg@FreeBSD.org>
// Copyright 2020 Hiroaki Nakamura <hnakamur@gmail.com>
// All rights reserved.
//
// This code is derived from software contributed to The NetBSD Foundation
// by Jason R. Thorpe of the Numerical Aerospace Simulation Facility,
// NASA Ames Research Center, by Luke Mewburn and by Tomas Svensson.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE NETBSD FOUNDATION, INC. AND CONTRIBUTORS
// ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
// PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR CONTRIBUTORS
// BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#[macro_use]
extern crate bitflags;

use std::fmt::{self, Write};
use std::result;
use thiserror::Error;

const MAX_SCALE: usize = 6;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Scale {
    Scale(usize),
    AutoScale,
    GetScale,
}

bitflags! {
    #[derive(Default)]
    pub struct Flags: u32 {
        const DECIMAL      = 0x01;
        const NOSPACE      = 0x02;
        const B            = 0x04;
        const DIVISOR_1000 = 0x08;
        const IEC_PREFIXES = 0x10;
    }
}

pub type Result<T> = result::Result<T, HumanizeNumberError>;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum HumanizeNumberError {
    #[error("scale must be less than or equal to 6")]
    TooBigScale,
    #[error("invalid combination of flags")]
    InvalidFlags,
    #[error("too small length")]
    TooSmallLength,
    #[error("format error")]
    FormatError(fmt::Error),
}

const PREFIXES_IEC_B: [&str; 7] = ["B", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei"];
const PREFIXES_IEC: [&str; 7] = ["", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei"];
const PREFIXES_1000_B: [&str; 7] = ["B", "k", "M", "G", "T", "P", "E"];
const PREFIXES_1000: [&str; 7] = ["", "k", "M", "G", "T", "P", "E"];
const PREFIXES_1024_B: [&str; 7] = ["B", "K", "M", "G", "T", "P", "E"];
const PREFIXES_1024: [&str; 7] = ["", "K", "M", "G", "T", "P", "E"];

pub fn humanize_number(
    w: &mut dyn fmt::Write,
    len: usize,
    mut quotient: i64,
    suffix: &str,
    scale: Scale,
    flags: Flags,
) -> Result<usize> {
    if let Scale::Scale(scale) = scale {
        if scale > MAX_SCALE {
            return Err(HumanizeNumberError::TooBigScale);
        }
    }
    if flags.contains(Flags::DIVISOR_1000) && flags.contains(Flags::IEC_PREFIXES) {
        return Err(HumanizeNumberError::InvalidFlags);
    }

    let mut remainder: i64 = 0;
    let divisor: i64;
    let divisordeccut: i64;
    let mut baselen: usize;
    let prefixes: &[&str];
    if flags.contains(Flags::IEC_PREFIXES) {
        baselen = 2;
        /*
         * Use the prefixes for power of two recommended by
         * the International Electrotechnical Commission
         * (IEC) in IEC 80000-3 (i.e. Ki, Mi, Gi...).
         *
         * HN_IEC_PREFIXES implies a divisor of 1024 here
         * (use of HN_DIVISOR_1000 would have triggered
         * an assertion earlier).
         */
        divisor = 1024;
        divisordeccut = 973; /* ceil(.95 * 1024) */
        if flags.contains(Flags::B) {
            prefixes = &PREFIXES_IEC_B[..];
        } else {
            prefixes = &PREFIXES_IEC[..];
        }
    } else {
        baselen = 1;
        if flags.contains(Flags::DIVISOR_1000) {
            divisor = 1000;
            divisordeccut = 950;
            if flags.contains(Flags::B) {
                prefixes = &PREFIXES_1000_B[..];
            } else {
                prefixes = &PREFIXES_1000[..];
            }
        } else {
            divisor = 1024;
            divisordeccut = 973; /* ceil(.95 * 1024) */
            if flags.contains(Flags::B) {
                prefixes = &PREFIXES_1024_B[..];
            } else {
                prefixes = &PREFIXES_1024[..];
            }
        }
    }

    let sign: i64;
    let sep: &str;
    if quotient < 0 {
        sign = -1;
        quotient = -quotient;
        baselen += 2; /* sign, digit */
    } else {
        sign = 1;
        baselen += 1; /* digit */
    }
    if flags.contains(Flags::NOSPACE) {
        sep = "";
    } else {
        sep = " ";
        baselen += 1;
    }
    baselen += suffix.len();

    /* Check if enough room for `x y' + suffix */
    if len < baselen {
        return Err(HumanizeNumberError::TooSmallLength);
    }

    let mut i: usize;
    match scale {
        Scale::Scale(scale) => {
            i = 0;
            while i < scale && i < MAX_SCALE {
                remainder = quotient % divisor;
                quotient /= divisor;
                i += 1;
            }
        }
        _ => {
            /* See if there is additional columns can be used. */
            let mut max: i64 = 1;
            if len > baselen {
                let mut i: usize = len - baselen;
                while i > 0 {
                    max *= 10;
                    i -= 1;
                }
            }

            /*
             * Divide the number until it fits the given column.
             * If there will be an overflow by the rounding below,
             * divide once more.
             */
            i = 0;
            while (quotient >= max
                || (quotient == max - 1
                    && (remainder >= divisordeccut || remainder >= divisor / 2)))
                && i < MAX_SCALE
            {
                remainder = quotient % divisor;
                quotient /= divisor;
                i += 1;
            }

            match scale {
                Scale::GetScale => return Ok(i),
                _ => (),
            }
        }
    }

    /* If a value <= 9.9 after rounding and ... */
    /*
     * XXX - should we make sure there is enough space for the decimal
     * place and if not, don't do HN_DECIMAL?
     */
    if ((quotient == 9 && remainder < divisordeccut) || quotient < 9)
        && i > 0
        && flags.contains(Flags::DECIMAL)
    {
        let s1 = quotient + ((remainder * 10 + divisor / 2) / divisor / 10);
        let s2 = ((remainder * 10 + divisor / 2) / divisor) % 10;
        const DECIMAL_POINT: &str = ".";
        if let Err(e) = write!(
            w,
            "{}{}{}{}{}{}",
            sign * s1,
            DECIMAL_POINT,
            s2,
            sep,
            prefixes[i],
            suffix
        ) {
            return Err(HumanizeNumberError::FormatError(e));
        }
    } else {
        if let Err(e) = write!(
            w,
            "{}{}{}{}",
            sign * (quotient + (remainder + divisor / 2) / divisor),
            sep,
            prefixes[i],
            suffix
        ) {
            return Err(HumanizeNumberError::FormatError(e));
        }
    }
    Ok(i)
}

pub struct DiscardWriter {
    n: usize,
}

impl DiscardWriter {
    pub fn new() -> Self {
        Self { n: 0 }
    }

    pub fn num_bytes_would_written(&self) -> usize {
        self.n
    }
}

impl Write for DiscardWriter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.n += s.len();
        Ok(())
    }
}

pub struct LimitedWriter<'a, T: ?Sized + 'a> {
    inner: &'a mut T,
    limit: usize,
    n: usize,
}

impl<'a, T> LimitedWriter<'a, T> {
    pub fn new(inner: &'a mut T, limit: usize) -> Self {
        Self { inner, limit, n: 0 }
    }

    pub fn num_bytes_would_written(&self) -> usize {
        self.n
    }
}

impl<T: Write + ?Sized> fmt::Write for LimitedWriter<'_, T> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let mut i = 0usize;
        while i < s.len() {
            let len = len_utf8_at(&s, i);
            if self.n + i + len > self.limit {
                break;
            }
            i += len;
        }
        self.inner.write_str(&s[0..i])?;
        self.n += s.len();
        Ok(())
    }
}

fn len_utf8_at(s: &str, i: usize) -> usize {
    let b = s.as_bytes()[i];
    if b & 0b1000_0000 == 0 {
        1
    } else if b & 0b1110_0000u8 == 0b1100_0000u8 {
        2
    } else if b & 0b1111_0000u8 == 0b1110_0000u8 {
        3
    } else if b & 0b1111_1000u8 == 0b1111_0000u8 {
        4
    } else {
        panic!("index not at char boundary: {}", i);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_len_utf8_at() {
        let s = "\u{0024}\u{00A2}\u{20AC}\u{10348}";
        assert_eq!(len_utf8_at(s, 0), 1);
        assert_eq!(len_utf8_at(s, 1), 2);
        assert_eq!(len_utf8_at(s, 3), 3);
        assert_eq!(len_utf8_at(s, 6), 4);
    }

    #[test]
    fn test_discard_writer() {
        let mut w = DiscardWriter::new();
        write!(&mut w, "\u{0024}\u{00A2}\u{20AC}\u{10348}").unwrap();
        assert_eq!(w.num_bytes_would_written(), 10);
    }

    #[test]
    fn test_limited_writer() {
        let mut buf = String::new();
        let mut w = LimitedWriter::new(&mut buf, 4);
        write!(&mut w, "\u{0024}\u{00A2}\u{20AC}\u{10348}").unwrap();
        assert_eq!(w.num_bytes_would_written(), 10);
        assert_eq!(buf, "\u{0024}\u{00A2}");

        buf.clear();
        let mut w = LimitedWriter::new(&mut buf, 3);
        write!(&mut w, "\u{0024}\u{00A2}\u{20AC}\u{10348}").unwrap();
        assert_eq!(w.num_bytes_would_written(), 10);
        assert_eq!(buf, "\u{0024}\u{00A2}");

        buf.clear();
        let mut w = LimitedWriter::new(&mut buf, 6);
        write!(&mut w, "\u{0024}\u{00A2}\u{20AC}\u{10348}").unwrap();
        assert_eq!(w.num_bytes_would_written(), 10);
        assert_eq!(buf, "\u{0024}\u{00A2}\u{20AC}");

        buf.clear();
        let mut w = LimitedWriter::new(&mut buf, 4);
        write!(&mut w, "1000 ").unwrap();
        assert_eq!(w.num_bytes_would_written(), 5);
        assert_eq!(buf, "1000");
    }

    #[test]
    #[should_panic(expected = "index not at char boundary: 7")]
    fn test_len_utf8_at_not_char_boundary() {
        const S: &str = "\u{0024}\u{00A2}\u{20AC}\u{10348}";
        len_utf8_at(S, 7);
    }

    #[test]
    fn test_humanize_number() {
        #[derive(Debug)]
        struct TestCase<'a> {
            want_res: Result<usize>,
            want_buf: &'a str,
            num: i64,
            flags: Flags,
            scale: Scale,
            len: usize,
        };

        impl<'a> TestCase<'a> {
            fn new(
                want_res: Result<usize>,
                want_buf: &'a str,
                num: i64,
                flags: Flags,
                scale: Scale,
                len: usize,
            ) -> Self {
                Self {
                    want_res,
                    want_buf,
                    num,
                    flags,
                    scale,
                    len,
                }
            }

            fn new_1000_auto(
                want_res: Result<usize>,
                want_buf: &'a str,
                num: i64,
                len: usize,
            ) -> Self {
                Self::new(
                    want_res,
                    want_buf,
                    num,
                    Flags::DIVISOR_1000,
                    Scale::AutoScale,
                    len,
                )
            }

            fn new_1024_auto(
                want_res: Result<usize>,
                want_buf: &'a str,
                num: i64,
                len: usize,
            ) -> Self {
                Self::new(
                    want_res,
                    want_buf,
                    num,
                    Default::default(),
                    Scale::AutoScale,
                    len,
                )
            }

            fn new_1000_scale(
                want_res: Result<usize>,
                want_buf: &'a str,
                num: i64,
                scale: usize,
                len: usize,
            ) -> Self {
                Self::new(
                    want_res,
                    want_buf,
                    num,
                    Flags::DIVISOR_1000,
                    Scale::Scale(scale),
                    len,
                )
            }

            fn new_1024_scale(
                want_res: Result<usize>,
                want_buf: &'a str,
                num: i64,
                scale: usize,
                len: usize,
            ) -> Self {
                Self::new(
                    want_res,
                    want_buf,
                    num,
                    Default::default(),
                    Scale::Scale(scale),
                    len,
                )
            }
        }

        let test_cases: [TestCase; 123] = [
            /* tests 0-13 test 1000 suffixes */
            TestCase::new_1000_auto(Ok(2), "0 ", 0, 4),
            TestCase::new_1000_auto(Ok(3), "1 k", 500, 4),
            TestCase::new_1000_auto(Ok(3), "1 M", 500_000, 4),
            TestCase::new_1000_auto(Ok(3), "1 G", 500_000_000, 4),
            TestCase::new_1000_auto(Ok(3), "1 T", 500_000_000_000, 4),
            TestCase::new_1000_auto(Ok(3), "1 P", 500_000_000_000_000, 4),
            TestCase::new_1000_auto(Ok(3), "1 E", 500_000_000_000_000_000, 4),
            TestCase::new_1000_auto(Ok(2), "1 ", 1, 4),
            TestCase::new_1000_auto(Ok(3), "2 k", 1_500, 4),
            TestCase::new_1000_auto(Ok(3), "2 M", 1_500_000, 4),
            TestCase::new_1000_auto(Ok(3), "2 G", 1_500_000_000, 4),
            TestCase::new_1000_auto(Ok(3), "2 T", 1_500_000_000_000, 4),
            TestCase::new_1000_auto(Ok(3), "2 P", 1_500_000_000_000_000, 4),
            TestCase::new_1000_auto(Ok(3), "2 E", 1_500_000_000_000_000_000, 4),
            /* tests 14-27 test 1024 suffixes */
            TestCase::new_1024_auto(Ok(2), "0 ", 0, 4),
            TestCase::new_1024_auto(Ok(3), "1 K", 512, 4),
            TestCase::new_1024_auto(Ok(3), "1 M", 512 * 1024, 4),
            TestCase::new_1024_auto(Ok(3), "1 G", 512 * 1024 * 1024, 4),
            TestCase::new_1024_auto(Ok(3), "1 T", 512 * 1024 * 1024 * 1024, 4),
            TestCase::new_1024_auto(Ok(3), "1 P", 512 * 1024 * 1024 * 1024 * 1024, 4),
            TestCase::new_1024_auto(Ok(3), "1 E", 512 * 1024 * 1024 * 1024 * 1024 * 1024, 4),
            TestCase::new_1024_auto(Ok(2), "1 ", 1, 4),
            TestCase::new_1024_auto(Ok(3), "2 K", 1536, 4),
            TestCase::new_1024_auto(Ok(3), "2 M", 1536 * 1024, 4),
            TestCase::new_1024_auto(Ok(3), "2 G", 1536 * 1024 * 1024, 4),
            TestCase::new_1024_auto(Ok(3), "2 T", 1536 * 1024 * 1024 * 1024, 4),
            TestCase::new_1024_auto(Ok(3), "2 P", 1536 * 1024 * 1024 * 1024 * 1024, 4),
            TestCase::new_1024_auto(Ok(3), "2 E", 1536 * 1024 * 1024 * 1024 * 1024 * 1024, 4),
            /* tests 28-37 test rounding */
            TestCase::new_1000_auto(Ok(3), "0 M", 500_000 - 1, 4),
            TestCase::new_1000_auto(Ok(3), "1 M", 500_000, 4),
            TestCase::new_1000_auto(Ok(3), "1 M", 1_500_000 - 1, 4),
            TestCase::new_1000_auto(Ok(3), "2 M", 1_500_000, 4),
            TestCase::new_1024_auto(Ok(3), "0 K", 512 - 1, 4),
            TestCase::new_1024_auto(Ok(3), "1 K", 512, 4),
            TestCase::new_1024_auto(Ok(3), "0 M", 512 * 1024 - 1, 4),
            TestCase::new_1024_auto(Ok(3), "1 M", 512 * 1024, 4),
            TestCase::new_1024_auto(Ok(3), "1 M", 1024 * 1024 + 512 * 1024 - 1, 4),
            TestCase::new_1024_auto(Ok(3), "2 M", 1024 * 1024 + 512 * 1024, 4),
            /* tests 38-61 test specific scale factors with 1000 divisor */
            TestCase::new_1000_scale(Ok(3), "0 k", 0, 1, 4),
            TestCase::new_1000_scale(Ok(3), "1 k", 500, 1, 4),
            TestCase::new_1000_scale(Ok(3), "0 M", 500, 2, 4),
            TestCase::new_1000_scale(Ok(3), "1 M", 500_000, 2, 4),
            TestCase::new_1000_scale(Ok(3), "0 G", 500_000, 3, 4),
            TestCase::new_1000_scale(Ok(3), "1 G", 500_000_000, 3, 4),
            TestCase::new_1000_scale(Ok(3), "0 T", 500_000_000, 4, 4),
            TestCase::new_1000_scale(Ok(3), "1 T", 500_000_000_000, 4, 4),
            TestCase::new_1000_scale(Ok(3), "0 P", 500_000_000_000, 5, 4),
            TestCase::new_1000_scale(Ok(3), "1 P", 500_000_000_000_000, 5, 4),
            TestCase::new_1000_scale(Ok(3), "0 E", 500_000_000_000_000, 6, 4),
            TestCase::new_1000_scale(Ok(3), "1 E", 500_000_000_000_000_000, 6, 4),
            TestCase::new_1000_scale(Ok(3), "0 k", 1, 1, 4),
            TestCase::new_1000_scale(Ok(3), "2 k", 1_500, 1, 4),
            TestCase::new_1000_scale(Ok(3), "0 M", 1_500, 2, 4),
            TestCase::new_1000_scale(Ok(3), "2 M", 1_500_000, 2, 4),
            TestCase::new_1000_scale(Ok(3), "0 G", 1_500_000, 3, 4),
            TestCase::new_1000_scale(Ok(3), "2 G", 1_500_000_000, 3, 4),
            TestCase::new_1000_scale(Ok(3), "0 T", 1_500_000_000, 4, 4),
            TestCase::new_1000_scale(Ok(3), "2 T", 1_500_000_000_000, 4, 4),
            TestCase::new_1000_scale(Ok(3), "0 P", 1_500_000_000_000, 5, 4),
            TestCase::new_1000_scale(Ok(3), "2 P", 1_500_000_000_000_000, 5, 4),
            TestCase::new_1000_scale(Ok(3), "0 E", 1_500_000_000_000_000, 6, 4),
            TestCase::new_1000_scale(Ok(3), "2 E", 1_500_000_000_000_000_000, 6, 4),
            /* tests 62-85 test specific scale factors with 1024 divisor */
            TestCase::new_1024_scale(Ok(3), "0 K", 0, 1, 4),
            TestCase::new_1024_scale(Ok(3), "1 K", 512, 1, 4),
            TestCase::new_1024_scale(Ok(3), "0 M", 512, 2, 4),
            TestCase::new_1024_scale(Ok(3), "1 M", 512 * 1024, 2, 4),
            TestCase::new_1024_scale(Ok(3), "0 G", 512 * 1024, 3, 4),
            TestCase::new_1024_scale(Ok(3), "1 G", 512 * 1024 * 1024, 3, 4),
            TestCase::new_1024_scale(Ok(3), "0 T", 512 * 1024 * 1024, 4, 4),
            TestCase::new_1024_scale(Ok(3), "1 T", 512 * 1024 * 1024 * 1024, 4, 4),
            TestCase::new_1024_scale(Ok(3), "0 P", 512 * 1024 * 1024 * 1024, 5, 4),
            TestCase::new_1024_scale(Ok(3), "1 P", 512 * 1024 * 1024 * 1024 * 1024, 5, 4),
            TestCase::new_1024_scale(Ok(3), "0 E", 512 * 1024 * 1024 * 1024 * 1024, 6, 4),
            TestCase::new_1024_scale(Ok(3), "1 E", 512 * 1024 * 1024 * 1024 * 1024 * 1024, 6, 4),
            TestCase::new_1024_scale(Ok(3), "0 K", 1, 1, 4),
            TestCase::new_1024_scale(Ok(3), "2 K", 1536, 1, 4),
            TestCase::new_1024_scale(Ok(3), "0 M", 1536, 2, 4),
            TestCase::new_1024_scale(Ok(3), "2 M", 1536 * 1024, 2, 4),
            TestCase::new_1024_scale(Ok(3), "0 G", 1536 * 1024, 3, 4),
            TestCase::new_1024_scale(Ok(3), "2 G", 1536 * 1024 * 1024, 3, 4),
            TestCase::new_1024_scale(Ok(3), "0 T", 1536 * 1024 * 1024, 4, 4),
            TestCase::new_1024_scale(Ok(3), "2 T", 1536 * 1024 * 1024 * 1024, 4, 4),
            TestCase::new_1024_scale(Ok(3), "0 P", 1536 * 1024 * 1024 * 1024, 5, 4),
            TestCase::new_1024_scale(Ok(3), "2 P", 1536 * 1024 * 1024 * 1024 * 1024, 5, 4),
            TestCase::new_1024_scale(Ok(3), "0 E", 1536 * 1024 * 1024 * 1024 * 1024, 6, 4),
            TestCase::new_1024_scale(Ok(3), "2 E", 1536 * 1024 * 1024 * 1024 * 1024 * 1024, 6, 4),
            /* tests 86-99 test invalid specific scale values of < 0 or >= 7 with
               and without HN_DIVISOR_1000 set */
            /*  all should return errors with new code; with old, the latter 3
                are instead processed as if having AUTOSCALE and/or GETSCALE set */
            TestCase::new_1024_scale(Err(HumanizeNumberError::TooBigScale), "", 1, 7, 4),
            TestCase::new_1000_scale(Err(HumanizeNumberError::TooBigScale), "", 1, 7, 4),
            TestCase::new_1024_scale(Err(HumanizeNumberError::TooBigScale), "", 1, 1000, 4),
            TestCase::new_1000_scale(Err(HumanizeNumberError::TooBigScale), "", 1, 1000, 4),
            TestCase::new_1024_scale(Err(HumanizeNumberError::TooBigScale), "", 1, 1000 * 1000, 4),
            TestCase::new_1000_scale(Err(HumanizeNumberError::TooBigScale), "", 1, 1000 * 1000, 4),
            TestCase::new_1024_scale(
                Err(HumanizeNumberError::TooBigScale),
                "",
                1,
                std::usize::MAX,
                4,
            ),
            TestCase::new_1000_scale(
                Err(HumanizeNumberError::TooBigScale),
                "",
                1,
                std::usize::MAX,
                4,
            ),
            // NOTE: No test cases for negative scale, since our scale type is usize.

            /* tests for scale == 0, without autoscale */
            /* tests 100-114 test scale 0 with 1000 divisor - print first N digits */
            // NOTE: In our rust version N == len, compared to C where N == len - 1
            // because of terminating '\0' character.
            TestCase::new_1000_scale(Ok(2), "0 ", 0, 0, 4),
            TestCase::new_1000_scale(Ok(2), "1 ", 1, 0, 4),
            TestCase::new_1000_scale(Ok(3), "10 ", 10, 0, 4),
            TestCase::new_1000_scale(Ok(3), "0 M", 150, 2, 4),
            TestCase::new_1000_scale(Ok(3), "0 M", 500, 2, 4),
            TestCase::new_1000_scale(Ok(3), "0 M", 999, 2, 4),
            TestCase::new_1000_scale(Ok(4), "150 ", 150, 0, 4),
            TestCase::new_1000_scale(Ok(4), "500 ", 500, 0, 4),
            TestCase::new_1000_scale(Ok(4), "999 ", 999, 0, 4),
            TestCase::new_1000_scale(Ok(5), "1000", 1000, 0, 4),
            TestCase::new_1000_scale(Ok(5), "1500", 1500, 0, 4),
            TestCase::new_1000_scale(Ok(7), "5000", 500_000, 0, 4),
            TestCase::new_1000_scale(Ok(8), "1500", 1_500_000, 0, 4),
            TestCase::new_1000_scale(Ok(10), "5000", 500_000_000, 0, 4),
            TestCase::new_1000_scale(Ok(11), "1500", 1_500_000_000, 0, 4),
            /* tests 115-126 test scale 0 with 1024 divisor - print first N digits */
            TestCase::new_1024_scale(Ok(2), "0 ", 0, 0, 4),
            TestCase::new_1024_scale(Ok(2), "1 ", 1, 0, 4),
            TestCase::new_1024_scale(Ok(3), "10 ", 10, 0, 4),
            TestCase::new_1024_scale(Ok(4), "150 ", 150, 0, 4),
            TestCase::new_1024_scale(Ok(4), "500 ", 500, 0, 4),
            TestCase::new_1024_scale(Ok(4), "999 ", 999, 0, 4),
            TestCase::new_1024_scale(Ok(5), "1000", 1000, 0, 4),
            TestCase::new_1024_scale(Ok(5), "1500", 1500, 0, 4),
            TestCase::new_1024_scale(Ok(7), "5000", 500_000, 0, 4),
            TestCase::new_1024_scale(Ok(8), "1500", 1_500_000, 0, 4),
            TestCase::new_1024_scale(Ok(10), "5000", 500_000_000, 0, 4),
            TestCase::new_1024_scale(Ok(11), "1500", 1_500_000_000, 0, 4),
            // 	/* Test case for rounding of edge numbers around 999.5+, see PR224498.
            // 	 * Require buflen >= 5 */
            // 	{ 4, "1.0M", (int64_t)1023500, HN_DECIMAL|HN_B|HN_NOSPACE, HN_AUTOSCALE, 5 },

            // 	/* Test boundary cases for very large positive/negative number formatting */
            // 	/* Explicit scale, divisor 1024 */

            // 	/* Requires buflen >= 6 */
            // 	{ 3, "8 E",   INT64_MAX, 0, 6, 6 },
            // 	{ 4, "-8 E", -INT64_MAX, 0, 6, 6 },
            // 	{ 3, "0 E", (int64_t)92*1024*1024*1024*1024*1024L, 0, 6, 6 },
            // 	{ 3, "0 E", -(int64_t)92*1024*1024*1024*1024*1024L, 0, 6, 6 },
            // 	{ 3, "0 E", (int64_t)82*1024*1024*1024*1024*1024L, 0, 6, 6 },
            // 	{ 3, "0 E", -(int64_t)82*1024*1024*1024*1024*1024L, 0, 6, 6 },
            // 	{ 3, "0 E", (int64_t)81*1024*1024*1024*1024*1024L, 0, 6, 6 },
            // 	{ 3, "0 E", -(int64_t)81*1024*1024*1024*1024*1024L, 0, 6, 6 },
            // 	{ 4, "92 P", (int64_t)92*1024*1024*1024*1024*1024L, 0, 5, 6 },
            // 	{ 5, "-92 P", -(int64_t)92*1024*1024*1024*1024*1024L, 0, 5, 6 },
            // 	{ 4, "82 P", (int64_t)82*1024*1024*1024*1024*1024L, 0, 5, 6 },
            // 	{ 5, "-82 P", -(int64_t)82*1024*1024*1024*1024*1024L, 0, 5, 6 },
            // 	{ 4, "81 P", (int64_t)81*1024*1024*1024*1024*1024L, 0, 5, 6 },
            // 	{ 5, "-81 P", -(int64_t)81*1024*1024*1024*1024*1024L, 0, 5, 6 },

            // 	/* Explicit scale, divisor 1000 */
            // 	{ 3, "9 E",   INT64_MAX, HN_DIVISOR_1000, 6, 6 },
            // 	{ 4, "-9 E", -INT64_MAX, HN_DIVISOR_1000,  6, 6 },
            // 	{ 3, "0 E", (int64_t)82*1024*1024*1024*1024*1024L, HN_DIVISOR_1000,  6, 6 },
            // 	{ 3, "0 E", -(int64_t)82*1024*1024*1024*1024*1024L, HN_DIVISOR_1000,  6, 6 },
            // 	{ 3, "0 E", (int64_t)82*1024*1024*1024*1024*1024L, HN_DIVISOR_1000,  6, 6 },
            // 	{ 3, "0 E", -(int64_t)82*1024*1024*1024*1024*1024L, HN_DIVISOR_1000,  6, 6 },
            // 	{ 4, "92 P", (int64_t)82*1024*1024*1024*1024*1024L, HN_DIVISOR_1000,  5, 6 },
            // 	{ 5, "-92 P", -(int64_t)82*1024*1024*1024*1024*1024L, HN_DIVISOR_1000,  5, 6 },
            // 	{ 4, "91 P", (int64_t)81*1024*1024*1024*1024*1024L, HN_DIVISOR_1000,  5, 6 },
            // 	{ 5, "-91 P", -(int64_t)81*1024*1024*1024*1024*1024L, HN_DIVISOR_1000,  5, 6 },

            // 	/* Autoscale, divisor 1024 */
            // 	{ 3, "8 E",   INT64_MAX, 0, HN_AUTOSCALE, 6 },
            // 	{ 4, "-8 E", -INT64_MAX, 0, HN_AUTOSCALE, 6 },
            // 	{ 4, "92 P", (int64_t)92*1024*1024*1024*1024*1024L, 0, HN_AUTOSCALE, 6 },
            // 	{ 5, "-92 P", -(int64_t)92*1024*1024*1024*1024*1024L, 0, HN_AUTOSCALE, 6 },
            // 	{ 4, "82 P", (int64_t)82*1024*1024*1024*1024*1024L, 0, HN_AUTOSCALE, 6 },
            // 	{ 5, "-82 P", -(int64_t)82*1024*1024*1024*1024*1024L, 0, HN_AUTOSCALE, 6 },
            // 	{ 4, "81 P", (int64_t)81*1024*1024*1024*1024*1024L, 0, HN_AUTOSCALE, 6 },
            // 	{ 5, "-81 P", -(int64_t)81*1024*1024*1024*1024*1024L, 0, HN_AUTOSCALE, 6 },
            // 	/* Autoscale, divisor 1000 */
            // 	{ 3, "9 E",   INT64_MAX, HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 4, "-9 E", -INT64_MAX, HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 4, "92 P", (int64_t)82*1024*1024*1024*1024*1024L, HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "-92 P", -(int64_t)82*1024*1024*1024*1024*1024L, HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 4, "91 P", (int64_t)81*1024*1024*1024*1024*1024L, HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "-91 P", -(int64_t)81*1024*1024*1024*1024*1024L, HN_DIVISOR_1000, HN_AUTOSCALE, 6 },

            // 	/* 0 scale, divisor 1024 */
            // 	{ 12, "skdj",  INT64_MAX, 0, 0, 6 },
            // 	{ 21, "-9223", -INT64_MAX, 0, 0, 6 },
            // 	{ 19, "10358", (int64_t)92*1024*1024*1024*1024*1024L, 0, 0, 6 },
            // 	{ 20, "-1035", -(int64_t)92*1024*1024*1024*1024*1024L, 0, 0, 6 },
            // 	{ 18, "92323", (int64_t)82*1024*1024*1024*1024*1024L, 0, 0, 6 },
            // 	{ 19, "-9232", -(int64_t)82*1024*1024*1024*1024*1024L, 0, 0, 6 },
            // 	{ 18, "91197", (int64_t)81*1024*1024*1024*1024*1024L, 0, 0, 6 },
            // 	{ 19, "-9119", -(int64_t)81*1024*1024*1024*1024*1024L, 0, 0, 6 },

            // 	/* 0 scale, divisor 1000 */
            // 	/* XXX - why does this fail? */
            // 	{ -1, "", INT64_MAX, HN_DIVISOR_1000, 0, 6 },
            // 	{ 21, "-9223", -INT64_MAX, HN_DIVISOR_1000,  0, 6 },
            // 	{ 19, "10358", (int64_t)92*1024*1024*1024*1024*1024L, HN_DIVISOR_1000,  0, 6 },
            // 	{ 20, "-1035", -(int64_t)92*1024*1024*1024*1024*1024L, HN_DIVISOR_1000,  0, 6 },
            // 	{ 18, "92323", (int64_t)82*1024*1024*1024*1024*1024L, HN_DIVISOR_1000,  0, 6 },
            // 	{ 19, "-9232", -(int64_t)82*1024*1024*1024*1024*1024L, HN_DIVISOR_1000,  0, 6 },
            // 	/* Expected to pass */
            // 	{ 18, "91197", (int64_t)81*1024*1024*1024*1024*1024L, HN_DIVISOR_1000,  0, 6 },
            // 	{ 19, "-9119", -(int64_t)81*1024*1024*1024*1024*1024L, HN_DIVISOR_1000,  0, 6 },

            /* tests for GETSCALE */
            TestCase::new(Ok(0), "", 0, Flags::DIVISOR_1000, Scale::GetScale, 6),
            TestCase::new(Ok(1), "", 1000, Flags::DIVISOR_1000, Scale::GetScale, 6),
            //         /* Tests for HN_DECIMAL */
            //         /* Positive, Autoscale */
            // 	{ 5, "500 k", (int64_t)500*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "994 k", (int64_t)994*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "995 k", (int64_t)995*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "999 k", (int64_t)999*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "1.0 M", (int64_t)1000*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "1.5 M", (int64_t)1500*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "1.9 M", (int64_t)1949*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "2.0 M", (int64_t)1950*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "9.9 M", (int64_t)9949*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 4, "10 M", (int64_t)9950*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "500 M", (int64_t)500*1000*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "994 M", (int64_t)994*1000*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "995 M", (int64_t)995*1000*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "999 M", (int64_t)999*1000*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },

            // 	{ 5, "500 K", (int64_t)500*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "994 K", (int64_t)994*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "995 K", (int64_t)995*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "999 K", (int64_t)999*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "1.0 M", (int64_t)1000*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "1.0 M", (int64_t)1018*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "1.0 M", (int64_t)1019*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "1.5 M", (int64_t)1536*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "1.9 M", (int64_t)1996*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "2.0 M", (int64_t)1997*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "2.0 M", (int64_t)2047*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "2.0 M", (int64_t)2048*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "2.0 M", (int64_t)2099*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "2.1 M", (int64_t)2100*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "9.9 M", (int64_t)10188*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	/* XXX - shouldn't the following two be "10. M"? */
            // 	{ 4, "10 M", (int64_t)10189*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 4, "10 M", (int64_t)10240*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "500 M", (int64_t)500*1024*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "994 M", (int64_t)994*1024*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "995 M", (int64_t)995*1024*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "999 M", (int64_t)999*1024*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "1.0 G", (int64_t)1000*1024*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "1.0 G", (int64_t)1023*1024*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },

            //         /* Negative, Autoscale - should pass */
            // 	{ 6, "-1.5 ", -(int64_t)1500*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 6, "-1.9 ", -(int64_t)1949*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 6, "-9.9 ", -(int64_t)9949*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },

            // 	{ 6, "-1.5 ", -(int64_t)1536*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 6, "-1.9 ", -(int64_t)1949*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 6, "-9.7 ", -(int64_t)9949*1024L, HN_DECIMAL, HN_AUTOSCALE, 6 },

            // 	/* Positive/negative, at maximum scale */
            // 	{ 5, "500 P", (int64_t)500*1000*1000*1000*1000*1000L, HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "1.9 E", (int64_t)1949*1000*1000*1000*1000*1000L, HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "8.9 E", (int64_t)8949*1000*1000*1000*1000*1000L, HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 5, "9.2 E", INT64_MAX, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            //         /* Negatives work with latest rev only: */
            // 	{ 6, "-9.2 ", -INT64_MAX, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },
            // 	{ 6, "-8.9 ", -(int64_t)8949*1000*1000*1000*1000*1000L, HN_DECIMAL|HN_DIVISOR_1000, HN_AUTOSCALE, 6 },

            // 	{ 5, "8.0 E",   INT64_MAX, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 5, "7.9 E",   INT64_MAX-(int64_t)100*1024*1024*1024*1024*1024LL, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 6, "-8.0 ", -INT64_MAX, HN_DECIMAL, HN_AUTOSCALE, 6 },
            // 	{ 6, "-7.9 ",   -INT64_MAX+(int64_t)100*1024*1024*1024*1024*1024LL, HN_DECIMAL, HN_AUTOSCALE, 6 },

            // 	/* Positive, Fixed scales */
            // 	{ 5, "500 k", (int64_t)500*1000L, HN_DECIMAL|HN_DIVISOR_1000, 1, 6 },
            // 	{ 5, "0.5 M", (int64_t)500*1000L, HN_DECIMAL|HN_DIVISOR_1000, 2, 6 },
            // 	{ 5, "949 k", (int64_t)949*1000L, HN_DECIMAL|HN_DIVISOR_1000, 1, 6 },
            // 	{ 5, "0.9 M", (int64_t)949*1000L, HN_DECIMAL|HN_DIVISOR_1000, 2, 6 },
            // 	{ 5, "950 k", (int64_t)950*1000L, HN_DECIMAL|HN_DIVISOR_1000, 1, 6 },
            // 	{ 5, "1.0 M", (int64_t)950*1000L, HN_DECIMAL|HN_DIVISOR_1000, 2, 6 },
            // 	{ 5, "999 k", (int64_t)999*1000L, HN_DECIMAL|HN_DIVISOR_1000, 1, 6 },
            // 	{ 5, "1.0 M", (int64_t)999*1000L, HN_DECIMAL|HN_DIVISOR_1000, 2, 6 },
            // 	{ 5, "1.5 M", (int64_t)1500*1000L, HN_DECIMAL|HN_DIVISOR_1000, 2, 6 },
            // 	{ 5, "1.9 M", (int64_t)1949*1000L, HN_DECIMAL|HN_DIVISOR_1000, 2, 6 },
            // 	{ 5, "2.0 M", (int64_t)1950*1000L, HN_DECIMAL|HN_DIVISOR_1000, 2, 6 },
            // 	{ 5, "9.9 M", (int64_t)9949*1000L, HN_DECIMAL|HN_DIVISOR_1000, 2, 6 },
            // 	{ 4, "10 M", (int64_t)9950*1000L, HN_DECIMAL|HN_DIVISOR_1000, 2, 6 },
            // 	{ 5, "500 M", (int64_t)500*1000*1000L, HN_DECIMAL|HN_DIVISOR_1000, 2, 6 },
            // 	{ 5, "0.5 G", (int64_t)500*1000*1000L, HN_DECIMAL|HN_DIVISOR_1000, 3, 6 },
            // 	{ 5, "999 M", (int64_t)999*1000*1000L, HN_DECIMAL|HN_DIVISOR_1000, 2, 6 },
            // 	{ 5, "1.0 G", (int64_t)999*1000*1000L, HN_DECIMAL|HN_DIVISOR_1000, 3, 6 },
            // 	/* Positive/negative, at maximum scale */
            // 	{ 5, "500 P", (int64_t)500*1000*1000*1000*1000*1000L, HN_DIVISOR_1000, 5, 6 },
            // 	{ 5, "1.0 E", (int64_t)500*1000*1000*1000*1000*1000L, HN_DIVISOR_1000, 6, 6 },
            // 	{ 5, "1.9 E", (int64_t)1949*1000*1000*1000*1000*1000L, HN_DIVISOR_1000, 6, 6 },
            // 	{ 5, "8.9 E", (int64_t)8949*1000*1000*1000*1000*1000L, HN_DIVISOR_1000, 6, 6 },
            // 	{ 5, "9.2 E", INT64_MAX, HN_DECIMAL|HN_DIVISOR_1000, 6, 6 },

            // 	/* HN_DECIMAL + binary + fixed scale cases not completed */
            // 	{ 5, "512 K", (int64_t)512*1024L, HN_DECIMAL, 1, 6 },
            // 	{ 5, "0.5 M", (int64_t)512*1024L, HN_DECIMAL, 2, 6 },

            // 	/* Negative, Fixed scales */
            //         /* Not yet added, but should work with latest rev */
        ];
        let mut buf = String::new();
        for (i, c) in test_cases.iter().enumerate() {
            let mut w = LimitedWriter::new(&mut buf, c.len);
            let res = humanize_number(&mut w, c.len, c.num, "", c.scale, c.flags);
            match res {
                Ok(n) => match c.scale {
                    Scale::GetScale => assert_eq!(
                        n,
                        *c.want_res.as_ref().unwrap(),
                        "case_index={}, test_case={:?}",
                        i,
                        c
                    ),
                    _ => assert_eq!(
                        w.num_bytes_would_written(),
                        *c.want_res.as_ref().unwrap(),
                        "case_index={}, test_case={:?}",
                        i,
                        c
                    ),
                },
                Err(e) => assert_eq!(&e, c.want_res.as_ref().err().unwrap()),
            }
            assert_eq!(&buf, c.want_buf, "case_index={}, test_case={:?}", i, c);
            buf.clear();
        }
    }

    #[test]
    fn test_too_big_scale() {
        let mut buf = String::new();
        let len = 5;
        let n = 1024;
        let res = humanize_number(&mut buf, len, n, "", Scale::Scale(7), Default::default());
        assert!(res.is_err());
        assert_eq!(res.err().unwrap(), HumanizeNumberError::TooBigScale);
    }

    #[test]
    fn test_invalid_flags() {
        let mut buf = String::new();
        let len = 5;
        let n = 1024;
        let res = humanize_number(
            &mut buf,
            len,
            n,
            "",
            Scale::AutoScale,
            Flags::DIVISOR_1000 | Flags::IEC_PREFIXES,
        );
        assert!(res.is_err());
        assert_eq!(res.err().unwrap(), HumanizeNumberError::InvalidFlags);
    }
}
