% http://www.math.washington.edu/~burke/crs/516/HTML/backtrack.html
% Backtracking Linesearch

function [xn,fn,fcall] = backtrack(xc,d,fc,fnc,DDfnc,c,gamma,eps)
%
%GENERAL DESCRIPTION
%
%This function performs the basic backtracking subroutine.
%The subroutine requires the following input:
%       xc    = the current point,
%       d     = the direction of search,
%       fc    = the current function value,
%       fnc   = a string variable for the function name,
%       DDfnc = the directional derivative of fnc at xc in the
%               direction d, must have  DDfnc < 0,
%       c     = the slope modification parameter in (0,1),
%       gamma = the backstepping parameter in (0,1),
%       eps   = the stopping criteria for norm(xn - xc),
%               that is, the main algorithm stops when
%               norm(xn - xc) <= eps.
%
%The routine returns
%       xn  = the new point,
%       fn  = the function value at the new point,
%       fnc = the number of calls to fnc.
%
%TERMINATION CRITERIA
%
%The backtracking is terminated if the step to the new point
%xn is so small that it triggers termination in the main algorithm,
%i.e. norm(xc - xn) <= eps. In this case we return xn = xc if
%fn >= fc (we have not reduced the function value); otherwise,
%we return xn.
%
%THE MATH
%
%The backtracking routing attempts to find a step size for
%reducing the value of the function fnc given the current point xc
%and a direction d. It does this by successively trying step sizes
%of the form gamma^s for s = 0,1,2... to find the smallest
%value of s for which the inequality
%
%       fnc(xc+gamma^s*d)\le fnc(xc)+c*gamma^s*DD
%
% is satisfied. The new point to be returned is then given
% by xn = xc+gamma^s*d.
%
%CHECK INPUT SPECIFICATIONS
%
if DDfnc >= 0,
        error('The backtracking subroutine has been sent a direction of nondesce
nt. Program has been terminated.')
end
if c<= 0 | c>= 1,
        error('The slope modification parameter c in the backtracking subroutine
 is not in (0,1).')
end
if gamma<=0 | gamma >=1,
        error('The backtracking parameter gamma is not in (0,1).')
end
if eps <= 0,
        error('The termination criteria eps sent to the backtracking line search
 is not positive.')
end

%
%CHECK DIMENSIONS
%
if size(xc)~=size(d)
        error('The vectors sent to backtrack are not of the same dimension.')
end

%
%
%EXECUTE THE LINE SEARCH
%
%

xn      =   xc+d;
cDDfnc  =   c*DDfnc;
fn      =   feval(fnc,xn);
fcall   =   1 ;

while fn > fc+cDDfnc,
        d       =  gamma*d;
        cDDfnc  =  gamma*cDDfnc;
        xn      =  xc+d;
        fn      =  feval(fnc,xn);
        fcall   =  fcall+1;

%Check if the step to xn is too small.
        if norm(d) <= eps,
        disp('linesearch step too small')
                if fn >= fc,
                        xn  =  xc;
                end
                break
        end
end


