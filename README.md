# Devon Emacs Extension

## Introduction

The Devon Emacs Extension is a powerful tool that integrates the Devon AI assistant into your Emacs environment. It provides an interactive interface for communicating with Devon, managing conversations, and leveraging AI assistance directly within your Emacs workflow.

## Installation

1. Clone this repository:
   ```
   git clone https://github.com/yourusername/devon-emacs.git
   ```

2. Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el` or `~/.emacs`):
   ```elisp
   (add-to-list 'load-path "/path/to/devon-emacs")
   (require 'devon)
   ```

3. Restart Emacs or evaluate the new configuration.

## Configuration

You can customize Devon's behavior by using `M-x customize-group RET devon RET` or by setting variables in your Emacs configuration file:

```elisp
(setq devon-backend-url "http://localhost")
(setq devon-default-port 8080)
(setq devon-auto-start-session t)
```

## Usage

1. Start a new Devon session:
   ```
   M-x devon-start-new-session
   ```

2. Use the Devon buffer to interact with the AI assistant. Type your messages and press Enter to send.

3. Use Devon commands (prefixed with `C-c d`) to manage your session and access various features.

## Key Features

- Interactive AI assistance within Emacs
- Conversation management and history
- Session saving and loading
- Syntax highlighting for Devon conversations
- Debug mode for troubleshooting
- Conversation export to JSON and Markdown
- Usage statistics and reporting
- Special handling and tracking of Checkpoint events

## Commands

Here are some of the most commonly used Devon commands:

- `C-c d s`: Start a new Devon session
- `C-c d q`: Exit the current session
- `C-c d r`: Restart the current session
- `C-c d h`: Display help (list of available commands)
- `C-c d w`: Display welcome message
- `C-c d i`: Display recent conversation history
- `C-c d S`: Save the current session
- `C-c d L`: Load a saved session
- `C-c d v`: Display Devon version information
- `C-c d c`: Load Devon configuration
- `C-c d u`: Update Devon configuration
- `C-c d g`: Generate usage report
- `C-c d C`: Clear the Devon buffer
- `C-c d E`: Export conversation to Markdown

## Customization

You can customize Devon's behavior using Emacs' built-in Customize interface:

```
M-x customize-group RET devon RET
```

This allows you to modify settings such as the backend URL, default port, and various display options.

## Troubleshooting

If you encounter issues:

1. Check the Devon backend status:
   ```
   M-x devon-check-backend
   ```

2. Enable debug mode for more detailed information:
   ```
   M-x devon-toggle-debug-mode
   ```

3. Review the `*Devon*` buffer for any error messages.

## Contributing

Contributions to the Devon Emacs Extension are welcome! Please fork the repository, make your changes, and submit a pull request.

For bug reports or feature requests, please open an issue on the GitHub repository.

---

For more information and updates, visit the [Devon Emacs Extension GitHub repository](https://github.com/yourusername/devon-emacs).
