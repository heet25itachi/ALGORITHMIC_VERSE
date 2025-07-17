syms k y(k)
diff_eq = y(k+1) - k * y(k) == 0;
sol = dsolve(diff_eq, y(0) == 1);
disp('Solution to difference equation:');
disp(sol);

% q-difference equation requires manual handling
disp('q-difference equations are not directly supported in MATLAB.');
