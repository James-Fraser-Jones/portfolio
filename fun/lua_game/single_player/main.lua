--the ONLY global variables
gameWidth = 1920
gameHeight = 1080

local Ss = require("modules.screenScale")
local game = {}

--------------------------------------------------------------------------------------------------------------------
--State Changing                            I CHANGED THIS FUNCTION TO BE GLOBAL SO I COULD USE IT IN THE MAIN MENU.. QUESTIONABLE
--I think that states actually need their own file but I don't know exactly how I want to implement this
function changeState(newState)
  game.states[game.currentState].kill()
  game.currentState = newState
  game.states[game.currentState].load()
end
--------------------------------------------------------------------------------------------------------------------
--Input Handler
function love.keypressed(k)
  local action = game.input[k]
  if action then
    return action()
  else
    local action = game.states[game.currentState].input[k]
    if action then return action() end
  end
end

function love.mousepressed(x, y, button)
  local k = ("mouse" .. button .. "down")
  local action = game.states[game.currentState].input[k]
  if action then
    --local newX, newY = sS.mouseTransform(x, y) (don't need these lines but they are the most generally correct)
    --return action(newX, newY)
    return action()
  end
end

function love.mousereleased(x, y, button)
  local k = ("mouse" .. button .. "up")
  local action = game.states[game.currentState].input[k]
  if action then
    --local newX, newY = sS.mouseTransform(x, y)
    return action()
  end
end
--------------------------------------------------------------------------------------------------------------------
--Initialisation of variables for initial state
function love.load()

  game = {
    states = {
      play = require("states.play"),
      mainMenu = require("states.mainMenu")
    },
    currentState = "mainMenu",
    input = {
      escape = love.event.quit,
      s = function() if game.currentState == "mainMenu" then changeState("play") else changeState("mainMenu") end end
    }
  }

  local width, height = love.graphics.getDimensions()
  Ss.load(gameWidth, gameHeight, width, height)

  game.states[game.currentState].load()
end
-------------------------------------------------------------------------------------------------------------------
--Update of game state
function love.update(dt)
  game.states[game.currentState].update(dt)
end
-------------------------------------------------------------------------------------------------------------------
--Drawing of game state
function love.draw()
  --general transformations to fit game to window size
  Ss.draw()
    --drawing of current game state
    game.states[game.currentState].draw()

  --resetting of transformations and settings
  love.graphics.setColor(255,255,255) --reset colour
  love.graphics.origin() --reset transformations
end
-------------------------------------------------------------------------------------------------------------------
--Dynamic scaling of game with window size
function love.resize(width, height)
  Ss.load(gameWidth, gameHeight, width, height)
end
