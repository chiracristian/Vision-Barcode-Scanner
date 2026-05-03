# Vision-Barcode-Scanner

A specialized computer vision and signal processing engine designed to decode **EAN-13** barcodes from raw image data. The system is built using a purely functional paradigm in **Scala**, ensuring mathematical precision and robust data transformation.

## Technical Pipeline
The scanner processes visual data through a multi-stage transformation pipeline:

1.  **Image Acquisition**: Parses binary `.ppm` (P6) color images into a structured RGB matrix.
2.  **Luminance Transformation**: Converts color data into greyscale using the weighted luminance formula: 
    $$Y = 0.3R + 0.59G + 0.11B$$
3.  **Adaptive Thresholding**: Generates a 1-bit black and white (PBM) image by calculating local pivots for distinct image quadrants, ensuring resilience against varied lighting conditions.
4.  **Signal Decoding**: Extracts and verifies numeric data from the processed bitstream using the EAN-13 specification.

## Core Engine Features
The heart of the system is the **Decoder logic**, which utilizes advanced functional programming concepts to ensure accuracy:

*   **Rational Arithmetic**: Implements a custom `RatioInt` class for exact rational math, avoiding the precision loss inherent in floating-point operations during signal scaling.
*   **Run-Length Encoding (RLE)**: Efficiently compresses raw bitstreams into width-encoded sequences for pattern analysis.
*   **Pattern-Matching & Parity**: Identifies **L/G/R-string** encodings and uses parity patterns to resolve the leading digit of the barcode.
*   **Best-Match Heuristics**: Employs a distance-calculation metric to identify digits with the highest statistical probability, providing robustness against image noise.
*   **Checksum Verification**: Implements the standard EAN-13 control weight algorithm to validate the final numeric string.

## Project Structure & Authorship

### Original Work
*   **`src/main/scala/Decoder.scala`**: This file represents my original work. It contains the core decoding engine, including the EAN-13 logic, rational math implementation, and signal verification algorithms.

### Support Framework
*   **`Convertor.scala`**, **`Parser.scala`**, **`Main.scala`**, and **`Types.scala`**: These components were developed by the **Programming Paradigms team** to provide the image-processing utilities and execution framework for this project.

## Execution
The project is managed via the **Scala Build Tool (sbt)**.

1.  **Input**: Place `.ppm` images in the `MyBarcodesInput` directory.
2.  **Build and Run**:
    ```bash
    sbt run
    ```
3.  **Output**: Decoded strings are printed to the console, and processed 1-bit images are saved in the `MyBarcodesOutput` directory for verification.

## Licensing
*   The original core logic contained in **`src/main/scala/Decoder.scala`** is licensed under the **BSD 3-Clause License**. See the header of that file for the full license text.
*   All other files in the repository are the property of the **Programming Paradigms team** within the *Faculty of Automatic Control and Computer Science* from *National University of Science and Technology Politehnica Bucharest*
