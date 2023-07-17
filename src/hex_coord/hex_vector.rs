use std::ops::{Add, AddAssign, Mul, MulAssign, Neg, Sub, SubAssign};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Default)]
pub struct HexVector {
    q: isize,
    r: isize,
}

impl HexVector {
    pub const fn new_cube(q: isize, r: isize, s: isize) -> Option<Self> {
        if q + r + s == 0 {
            Some(HexVector { q, r })
        } else {
            None
        }
    }

    pub const fn new_axial(q: isize, r: isize) -> Self {
        HexVector { q, r }
    }

    pub const fn get_q(self) -> isize {
        self.q
    }

    pub const fn get_r(self) -> isize {
        self.r
    }

    pub const fn get_s(self) -> isize {
        -self.q - self.r
    }

    pub fn mag(self) -> usize {
        self.get_s()
            .unsigned_abs()
            .max(self.q.unsigned_abs())
            .max(self.r.unsigned_abs())
    }

    fn gcd2(mut n: usize, mut m: usize) -> usize {
        if n == 0 || m == 0 {
            return 1;
        }
        while m != 0 {
            if m < n {
                std::mem::swap(&mut m, &mut n);
            }
            m %= n;
        }
        n
    }

    fn gcd3(q: isize, r: isize) -> usize {
        let s = (-q - r).unsigned_abs();
        let q = q.unsigned_abs();
        let r = r.unsigned_abs();

        Self::gcd2(Self::gcd2(q, r), s)
    }

    pub fn gcd(self) -> isize {
        Self::gcd3(self.q, self.r) as isize
    }

    pub fn normalize(mut self) -> Self {
        let gcd = self.gcd();

        self.q /= gcd;
        self.r /= gcd;

        self
    }

    pub fn distance(mut self, rhs: Self) -> usize {
        self -= rhs;
        self.mag()
    }

    pub fn rotate_clock(self) -> Self {
        Self::new_axial(-self.r, self.q + self.r)
    }

    pub fn rotate_anti(self) -> Self {
        Self::new_axial(self.q + self.r, -self.q)
    }

    pub const fn neg(self) -> Self {
        let q = -self.q;
        let r = -self.r;
        HexVector { q, r }
    }
}

impl AddAssign for HexVector {
    fn add_assign(&mut self, rhs: Self) {
        self.q += rhs.q;
        self.r += rhs.r;
    }
}

impl Add for HexVector {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;
        self
    }
}

impl Neg for HexVector {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::neg(self)
    }
}

impl SubAssign for HexVector {
    fn sub_assign(&mut self, rhs: Self) {
        self.q -= rhs.q;
        self.r -= rhs.r;
    }
}

impl Sub for HexVector {
    type Output = Self;

    fn sub(mut self, rhs: Self) -> Self::Output {
        self -= rhs;
        self
    }
}

impl MulAssign<isize> for HexVector {
    fn mul_assign(&mut self, rhs: isize) {
        self.q *= rhs;
        self.r *= rhs;
    }
}

impl Mul<isize> for HexVector {
    type Output = Self;

    fn mul(mut self, rhs: isize) -> Self::Output {
        self *= rhs;
        self
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for HexVector {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        (self.q, self.r).serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for HexVector {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let (q, r) = serde::Deserialize::deserialize(deserializer)?;
        Ok(HexVector::new_axial(q, r))
    }
}
