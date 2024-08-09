# Devon Emacs Extension

*This is still work-in-progress and very unstable.*

## Introduction

The Devon Emacs Extension is a powerful tool that integrates the Devon AI assistant into your Emacs environment. It provides an interactive interface for communicating with Devon, managing conversations, and leveraging AI assistance directly within your Emacs workflow.

## Installation

1. Clone this repository:
   ```
   git clone https://github.com/systemfreund/devon-mode.git
   ```

2. Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el` or `~/.emacs`):
   ```elisp
   (add-to-list 'load-path "/path/to/devon-mode")
   (require 'devon)
   ```

3. Restart Emacs or evaluate the new configuration.

## Configuration

You can customize Devon's behavior by using `M-x customize-group RET devon RET` or by setting variables in your Emacs configuration file:

## Usage

1. Start a new Devon session:
   ```
   M-x devon-create-session
   ```
