local UI = {} --table of functions to be called by other lua scripts (in this case my game state scripts) that wish to use this library
local states = {}

local function mouseOverButton(state)
  local x, y = states[state].mousePosition[1], states[state].mousePosition[2]
  local buttons = states[state].buttons
  for name, button in pairs(buttons) do
    if x >= button.location[1] and x <= button.location[1] + button.dimensions[1] and y >= button.location[2] and y <= button.location[2] + button.dimensions[2] then
      return name, button
    end
  end
  return "NONE", false
end

local function drawRoundedBox(x, y, w, h, r)

  -- where r <= math.min(w,h)/2

  local segments = 15

  ---[[
  --draw the main rectangle
  love.graphics.rectangle("fill", x+r, y, w-2*r, h)
  --draw the other two little ones either side
  love.graphics.rectangle("fill", x, y+r, r, h-2*r)
  love.graphics.rectangle("fill", x+w-r, y+r, r, h-2*r)
  --]]

  ---[[
  --draw all four corners as arcs of a specific radius
  love.graphics.arc("fill", "pie", x+w-r, y+h-r, r,           0, math.pi/2  , segments)
  love.graphics.arc("fill", "pie",   x+r, y+h-r, r,   math.pi/2, math.pi    , segments)
  love.graphics.arc("fill", "pie",   x+r,   y+r, r,     math.pi, 3*math.pi/2, segments)
  love.graphics.arc("fill", "pie", x+w-r,   y+r, r, 3*math.pi/2, 2*math.pi  , segments)
  --]]
end

------------------------------------------------------------------------------------------------

UI.load = function(state)
  states[state] = {
    buttons = {},
    canvas = love.graphics.newCanvas(gameWidth, gameHeight),
    currentButton = {"NONE", false},
    mousePosition = {}
  }
end

UI.update = function(state, x, y)

  states[state].mousePosition[1] = x
  states[state].mousePosition[2] = y

  states[state].currentButton[2] = false

  if states[state].currentButton[1] then
    local name, button = mouseOverButton(state)
    if name == states[state].currentButton[1] and name ~= "NONE" then
      states[state].currentButton[2] = true
    end
  end

end

UI.draw = function(state)
  love.graphics.draw(states[state].canvas)

  if states[state].currentButton[2] == true then

    local button = states[state].buttons[states[state].currentButton[1]]
    local r,g,b = unpack(button.color)
    love.graphics.setColor(r - 15, g - 15, b - 15)
    --love.graphics.rectangle("fill", button.location[1], button.location[2], button.dimensions[1], button.dimensions[2])
    drawRoundedBox(button.location[1], button.location[2], button.dimensions[1], button.dimensions[2], 40)
    love.graphics.setColor(255, 255, 255)
    love.graphics.draw(button.text, button.location[1], button.location[2], 0, button.scale, button.scale, button.xOffset, button.yOffset)

  end

  --[[
  local button = states[state].buttons.button2
  local r,g,b = unpack(button.color)
  love.graphics.setColor(r/2, g/2, b/2)
  --love.graphics.rectangle("fill", button.location[1], button.location[2], button.dimensions[1], button.dimensions[2])
  drawRoundedBox(button.location[1], button.location[2], button.dimensions[1], button.dimensions[2], math.min(button.dimensions[1], button.dimensions[2])/2*states[state].mousePosition[1]/gameWidth)
  love.graphics.setColor(255, 255, 255)
  love.graphics.draw(button.text, button.location[1], button.location[2], 0, button.scale, button.scale, button.xOffset, button.yOffset)
  --]]

end

UI.createButton = function(state, name, x, y, width, height, r, g, b, text, lClick, rClick)
  local button = {
    location = {x, y},
    dimensions = {width, height},
    color = {r, g, b},
    text = text,
    leftClick = lClick,
    rightClick = rClick
  }

  local arial = love.graphics.newFont("arial.ttf", 200)
  button.text = love.graphics.newText(arial, button.text)

  local textWidth, textHeight = button.text:getDimensions()
  button.scale = math.min(button.dimensions[1]/textWidth, button.dimensions[2]/textHeight) * 0.8
  button.xOffset = -((button.dimensions[1]/button.scale)-textWidth)/2
  button.yOffset = -((button.dimensions[2]/button.scale)-textHeight)/2

  love.graphics.setCanvas(states[state].canvas)
    love.graphics.setColor(unpack(button.color))
    --love.graphics.rectangle("fill", button.location[1], button.location[2], button.dimensions[1], button.dimensions[2])
    drawRoundedBox(button.location[1], button.location[2], button.dimensions[1], button.dimensions[2], 40)
    love.graphics.setColor(255, 255, 255)
    love.graphics.draw(button.text, button.location[1], button.location[2], 0, button.scale, button.scale, button.xOffset, button.yOffset)
  love.graphics.setCanvas()

  states[state].buttons[name] = button
end

UI.mousePressed = function(state, lr)

  local name = mouseOverButton(state)
  states[state].currentButton[1] = name

end

UI.mouseReleased = function(state, lr)

  local name, button = mouseOverButton(state)

  if name == states[state].currentButton[1] and name ~= "NONE" then
    local action = button[lr]
    if action then action() end
  end

  states[state].currentButton[1] = "NONE"

end

return UI
