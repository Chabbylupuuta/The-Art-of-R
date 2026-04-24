# 🖥️ R GUI Application for Credit Control Anomaly Detection

[![R Version](https://img.shields.io/badge/R-%E2%89%A54.0-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Status](https://img.shields.io/badge/status-active-brightgreen.svg)]()

> A point‑and‑click desktop application built with **[tkinter / gWidgets2 / Shiny]** to automatically detect anomalies in customer financial statements and support credit control decisions.

---

## 📋 Table of Contents

- [About the Project](#-about-the-project)
- [✨ Features](#-features)
- [📸 Screenshots](#-screenshots)
- [🚀 Getting Started](#-getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
- [💻 Usage](#-usage)
- [📁 Input File Format](#-input-file-format)
- [📤 Output Files](#-output-files)
- [🛠️ Built With](#️-built-with)
- [🤝 Contributing](#-contributing)
- [📄 License](#-license)
- [📧 Contact](#-contact)

---

## 🧾 About the Project

Managing customer credit manually is error‑prone and time‑consuming. This **R‑based GUI application** loads an Excel statement, applies intelligent anomaly detection rules (unusual amounts, payment gaps, negative balance, rapid balance jumps), and produces a clean report with visualisations – all without writing a single line of code.

Perfect for **finance teams**, **credit controllers**, and **small business owners** who use R as a backend but want a simple graphical interface.

---

## ✨ Features

- 📂 **Load Excel files** with one click – supports `.xlsx` and `.xls`
- 🔍 **Automatic anomaly detection**:
  - Unusually high/low invoice amounts (IQR method)
  - Unusual payment amounts
  - Long gaps without payment (>45 days)
  - Negative balance (overpayment)
  - Rapid balance increase (>50% month‑over‑month)
- 📊 **Interactive balance chart** with flagged anomalies
- 📑 **Export CSV report** and **save plot as PNG**
- 🧹 **One‑click data cleaning** – handles header rows and date parsing
- 🧠 **No coding required** – all logic runs behind a friendly GUI

---

## 📸 Screenshots

> *Replace these placeholders with actual screenshots of your GUI*

| Main Window | Anomaly Report |
|-------------|----------------|
| ![Main GUI](screenshots/main_window.png) | ![Report](screenshots/report_view.png) |

---

## 🚀 Getting Started

Follow these instructions to get the application running on your local machine.

### Prerequisites

- **R** (version 4.0 or later) – [Download R](https://cran.r-project.org/)
- **RStudio** (recommended, but optional)

### Installation

1. **Clone the repository**
   ```bash
   git clone https://github.com/your-username/your-gui-repo.git
   cd your-gui-repo
