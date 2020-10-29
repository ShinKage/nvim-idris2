-- Compute prime factorization of a number, using the bigint library
--
-- Based on Ray Gardner's work (public domain) from 1985, and 
-- Thad Smith's work in 1989.
-- 
-- Placed in the public domain, Jorj Bauer, 2016.

require "std.strict"

local bigint = require "bigint"

local factor = {}

function factor.compute(n)
   local prevfact = bigint:new(0)
   n = bigint:new(n) -- ensure it's a bigint

   local results = {}

   local function show(d,k)
      prevfact = prevfact + 1

      table.insert(results, d)

      if (k > bigint:new(1)) then
	 -- exponentiation: we'll return that as repeating factors
	 local i=bigint:new(1)
	 while (i <= k-1) do
	    table.insert(results, d)
	    i = i + 1
	 end
      end
   end

   local d = bigint:new(0)
   local k = bigint:new(0)
   
   prevfact = 0
   d = n + 1
   -- check for integer rollover. If we find it, we know the math library can't
   -- handle math on numbers this large.
   if (n+bigint:new(3) ~= d+bigint:new(2)) then
      error(n .. " is too large to process.")
   end

   if (n < bigint:new(2)) then
      error(n .. " is less than 2")
   elseif (n > bigint:new(2)) then
      d = bigint:new(2)
      k = bigint:new(0)
      while (n % d == bigint:new(0)) do
	 n = n / d
	 k = k + 1
      end
      if (k > bigint:new(0)) then
	 show(d,k)
      end

      d = bigint:new(3)
      while (d * d <= n) do
	 k=bigint:new(0)
	 while (n % d == bigint:new(0)) do 
	    n = n / d
	    k = k + 1
	 end
	 if (k > bigint:new(0)) then
	    show(d,k)
	 end
	 d = d + 2
      end
   end

   if (n > bigint:new(1)) then
      if (prevfact == bigint:new(0)) then
	 -- It's prime.
	 return { n }
      else
	 show(n,bigint:new(1))
      end
   end

   return results
end

return factor
