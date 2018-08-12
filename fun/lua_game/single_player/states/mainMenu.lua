local UI = require("modules.userInterface")
local Ss = require("modules.screenScale")
--------------------------------------------------------------------------------------------------------------------
local mainMenu = {}
local data = {}

mainMenu.load = function()
  data.color = {39, 161, 221}

  data.background = love.graphics.newCanvas(gameWidth, gameHeight)
  love.graphics.setCanvas(data.background)
    love.graphics.setColor(unpack(data.color))
    love.graphics.rectangle("fill", 0, 0, gameWidth, gameHeight)
  love.graphics.setCanvas()

  UI.load("mainMenu")
  UI.createButton("mainMenu", "button1", 100, 100, 200, 100, 22, 86, 188, "button1", function() changeState("play") end, function () print("button1 rightclicked") end)
  UI.createButton("mainMenu", "button2", 350, 100, 300, 150, 188, 136, 13, "another button", function () print("button2 leftclicked") end)
end

mainMenu.update = function(dt)
  local x, y = love.mouse.getPosition()
  local newX, newY = Ss.reverse(x, y)
  UI.update("mainMenu", newX, newY)
end

mainMenu.draw = function()
  love.graphics.draw(data.background)
  UI.draw("mainMenu")
  love.graphics.print("Main Menu!", gameWidth/2, gameHeight/2, 0, 2, 2)
end

mainMenu.input = {
  mouse1up = function() UI.mouseReleased("mainMenu", "leftClick") end,
  mouse2up = function() UI.mouseReleased("mainMenu", "rightClick") end,
  mouse1down = function() UI.mousePressed("mainMenu", "leftClick") end,
  mouse2down = function() UI.mousePressed("mainMenu", "rightClick") end,
}

mainMenu.kill = function()

end

return mainMenu
