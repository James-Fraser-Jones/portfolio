-------------------------------------------------------------------------------------------------------------------
--Initialisation code, creation of world and objects within it

--declaration of network variables
local socket = require("socket")
local udp = socket.udp()
udp:setpeername("192.168.1.70", 6000)
local updateRate = 0.06 --declaration of updateRate and timer objects (client updateRate should be slightly larger than server's to prevent backlog of udp packets to server
local timer = 0

--declaration of global Initialisation variables
local players = {}
local myPlayer = "player2"

--declaration of non-global Initialisation variables
function love.load()
  local player2 = {
    name = "player2",
    lr = 0,
    ud = 0
  }
  players[player2.name] = player2

  timer = love.timer.getTime() --set timer
end
-------------------------------------------------------------------------------------------------------------------
--Update of game state
function love.update(dt)

  --peroidically get updates for each player's input states lr, ud
  if love.timer.getTime() - timer > updateRate then

    timer = love.timer.getTime() --reset timer

    --update for player2 (client player)
    players[myPlayer].lr = 0
    players[myPlayer].ud = 0
    if love.keyboard.isDown("right") then players[myPlayer].lr = players[myPlayer].lr + 1 end
    if love.keyboard.isDown("left" ) then players[myPlayer].lr = players[myPlayer].lr - 1 end
    if love.keyboard.isDown("up"   ) then players[myPlayer].ud = players[myPlayer].ud + 1 end
    if love.keyboard.isDown("down" ) then players[myPlayer].ud = players[myPlayer].ud - 1 end

    --send input information to server
    udp:send(players[myPlayer].lr .. "," .. players[myPlayer].ud)
  end
end

function love.draw()
  love.graphics.print("lr = " .. players[myPlayer].lr, 25, 25, 0, 2, 2)
  love.graphics.print("ud = " .. players[myPlayer].ud, 25, 50, 0, 2, 2)
end
-------------------------------------------------------------------------------------------------------------------
--Input Handler
local keys = {
  escape = love.event.quit
}

function love.keypressed(k)
  local action = keys[k]
  if action then return action() end
end
