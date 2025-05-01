# Geometric Algebra paclet

Check it out in [Paclet Repository](https://resources.wolframcloud.com/PacletRepository/resources/Wolfram/GeometricAlgebra/).

## Installation


Install from the paclet repository:

```
PacletInstall["Wolfram/GeometricAlgebra"]
```

Or locally:

```
PacletDirectoryLoad["path_to_paclet_directory"]
```

## Load and use

```
<< Wolfram`GeometricAlgebra`
```

```
e = GeometricAlgebra[3];

r = 2 e[1] + 3 e[2] + 4 e[3]

r ** e[1, 2]
```