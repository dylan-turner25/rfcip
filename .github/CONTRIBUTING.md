# Contributing to rfcip

Thank you for considering contributing to rfcip! This package helps users access Federal Crop Insurance Program data, and we welcome contributions that improve functionality, documentation, or usability.

## Types of Contributions

We welcome several types of contributions:

- **Bug reports**: Found something that doesn't work as expected? Please let us know.
- **Feature requests**: Have an idea for new functionality? We'd love to hear it.
- **Code contributions**: Bug fixes, new features, performance improvements.
- **Documentation improvements**: Better examples, clearer explanations, typo fixes.

## Getting Started

1. Fork the repository on GitHub
2. Clone your fork locally:
   ```bash
   git clone https://github.com/yourusername/rfcip.git
   cd rfcip
   ```
3. Create a branch for your changes:
   ```bash
   git checkout -b feature/your-feature-name
   ```

## Development Setup

1. Install development dependencies:
   ```r
   install.packages("devtools")
   devtools::install_dev_deps()
   ```

2. Run tests to ensure everything works:
   ```r
   devtools::test()
   ```

3. Check package integrity:
   ```r
   devtools::check()
   ```

## Making Changes

### Code Style

- Follow existing code style and conventions in the package
- Use meaningful variable and function names
- Add documentation for new functions using roxygen2 comments
- Include examples in function documentation where appropriate

### Testing

- Add tests for new functionality using testthat
- Ensure all existing tests pass before submitting
- Aim for good test coverage of new code

### Documentation

- Update documentation for any changed functionality
- Add examples to demonstrate new features
- Update the README if adding significant new functionality

## Submitting Changes

1. Commit your changes with clear, descriptive commit messages:
   ```bash
   git commit -m "Add function to retrieve livestock data"
   ```

2. Push to your fork:
   ```bash
   git push origin feature/your-feature-name
   ```

3. Submit a pull request on GitHub with:
   - Clear description of what the changes do
   - Reference to any related issues
   - Screenshots or examples if relevant

## Pull Request Guidelines

- Keep pull requests focused on a single issue or feature
- Include tests for new functionality
- Update documentation as needed
- Ensure `devtools::check()` passes without errors or warnings
- Be responsive to code review feedback

## Reporting Issues

When reporting bugs or requesting features, please:

- Use a clear, descriptive title
- Provide steps to reproduce the issue (for bugs)
- Include your R version and package versions
- Include a minimal reproducible example if possible

## Questions?

If you have questions about contributing, feel free to:
- Open an issue for discussion
- Contact the maintainer: Dylan Turner <dylan.turner3@outlook.com>

Thank you for helping make rfcip better!