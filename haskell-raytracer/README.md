# Haskell Ray Tracer

This project is a Haskell implementation of the "Ray Tracing in One Weekend" tutorial, which you can find [here](https://raytracing.github.io/books/RayTracingInOneWeekend.html). It demonstrates the fundamentals of ray tracing and computer graphics, translated from C++ to Haskell.

## Project Structure

The project is organized into several directories and files:

- `src/`
  - `Color.hs`
  - `Vec3.hs`
  - `Ray.hs`
  - `Camera.hs`
  - `VectorConstants.hs`
  - `Interval.hs`
  - `Hittable.hs`
  - `HittableList.hs`
  - `Sphere.hs`
- `test/`
  - `Specs.hs`
- `app/`
  - `Main.hs`

## Purpose

The purpose of this project is twofold:
1. To explore computer graphics and vision by implementing a ray tracer.
2. To develop and enhance Haskell programming skills by converting an object-oriented program written in C++ into a functional program with the same feature set.

## Challenges

Converting the tutorial's C++ code to Haskell presented several challenges:
- Adapting object-oriented concepts to a functional programming paradigm.
- Learning how to manage state and side effects in Haskell.

## Performance Considerations

**High-Resolution Rendering:**

The high-resolution rendering can be very slow and resource-intensive. It took me about 3 hours to render the high resolution scene on my personal computer with the following specs:
1. `CPU: AMD Ryzen 5 3600X (6 cores 12 threads)`
2. `GPU: Nvidia Titan V`
3. `32.0 GB RAM`

If you find that the high-resolution render takes too long, you can use just the lower-resolution test case instead or edit the camera qualities for the high resolution scene to render at a lower, but still high quality. To do this:
1. Open the `Specs.hs` file located in the `test` directory.
2. Comment out the high-resolution render test code or edit the highResCamera arguments as needed.

## Setup and Usage

1. Clone the repository:
    ```sh
    git clone https://github.com/yourusername/haskell-ray-tracer.git
    cd haskell-raytracer
    ```

2. Build the project:
    ```sh
    stack build
    ```

3. Run the project:
    ```sh
    stack exec haskell-raytracer
    ```

4. Run tests:
    ```sh
    stack test
    ```

## Learning Outcomes

Working on this project provided a deeper understanding of:
- Functional programming principles and Haskell syntax.
- Ray tracing algorithms and computer graphics concepts.
- Translating object-oriented designs to functional programming.

## Acknowledgements

- Thanks to Peter Shirley for the excellent ["Ray Tracing in One Weekend"](https://raytracing.github.io/books/RayTracingInOneWeekend.html) tutorial.
- The Haskell community for their support and resources.
