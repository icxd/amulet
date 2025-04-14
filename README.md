# amulet

![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/icxd/amulet/rust.yml?style=for-the-badge)
![GitHub](https://img.shields.io/github/license/icxd/amulet?style=for-the-badge)
![Discord](https://img.shields.io/discord/1357315638409822208?label=Discord&style=for-the-badge)

A statically-typed, mainly object-oriented programming language written in Rust.

## 📖 Table of Contents

- [📖 Table of Contents](#-table-of-contents)
- [🛠️ Building](#-building)
- [👋🏻 Hello, World!](#-hello-world)
- [🤝🏻 Contributing](#-contributing)
- [📝 License](#-license) 

## 🛠️ Building

### Linux (Ubuntu)

```bash
sudo apt install llvm-18-dev clang-18 libpolly-18-dev
cargo build --release
```

### MacOS

```bash
brew install llvm
cargo build --release
```

## 👋🏻 Hello, World!

```rust
fn main() {
  println("Hello, World!");
}
```

## 🤝🏻 Contributing

Contributions are always welcome! Please feel free to open an issue or submit a pull request. If you're new to Rust, check out the [Rust Book](https://doc.rust-lang.org/book/) and [Rust By Example](https://doc.rust-lang.org/rust-by-example/).

## 📝 License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.