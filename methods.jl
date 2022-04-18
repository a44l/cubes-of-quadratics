

""" `tangent_space(quadratic)` 

Returns a matrix whose image is the tangent space to the length(quadratics)-th secant of the cubes of quadrics variety at `quadratics.^3`.
The tangent space is encoded with respect to the basis `monomials(X, 6)` of monomials.
"""
tangent_space = function(quadratics, vars)
    n = length(vars)
    monoms = monomials(vars, 6)
    coeffs = [sparse(coefficients(quadratic^2*vars[i]*vars[j], monoms)) for i in 1:n, j in 1:n, quadratic in quadratics if i≤j]
    return hcat(coeffs...)
end

""" `linear_space_equations(A, variables)` 

Takes a matrix A whose image is the linear space and returns a list of linear polynomials in variables `variables` that jointly vanish precisely on the image of A.
`variables` needs to have precisely as many entries as `size(A, 1)`.
"""
linear_space_equations = function(A)
    return nullspace(A')' # is a matrix B satisfying BA = 0
end