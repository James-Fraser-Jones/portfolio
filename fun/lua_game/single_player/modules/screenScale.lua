local Ss = {}  --table of functions to be called by other lua scripts (in this case my game state scripts) that wish to use this library
local data = {} --table to store data about the currently required scale and offset requirements until load function is called

Ss.load = function(gameWidth, gameHeight, windowWidth, windowHeight)
  data.scale = math.min(windowWidth/gameWidth, windowHeight/gameHeight)
  data.xOffset = ((windowWidth/data.scale)-gameWidth)/2
  data.yOffset = ((windowHeight/data.scale)-gameHeight)/2
end

Ss.draw = function()
  love.graphics.scale(data.scale, data.scale)
  love.graphics.translate(data.xOffset, data.yOffset)
end

Ss.reverse = function(x, y)
  local newX = x/data.scale - data.xOffset
  local newY = y/data.scale - data.yOffset
  return newX, newY
end

return Ss
