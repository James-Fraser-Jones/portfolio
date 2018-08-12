-------------------------------------------------------------------------------------------------------------------
--Initialisation code, creation of world and objects within it

--declaration of network variables
local socket = require("socket")
local udp = socket.udp()
udp:setsockname("*", 6000)
udp:settimeout(0) --prevent blocking of udp recieve function
local updateRate = 0.05 --declaration of updateRate and timer objects
local timer = 0

--declaration of global Initialisation variables (this must be at the top of the file!)
local world = {
  width = 1920, --you can change these
  height = 1080, --you can change these
  color = {9, 155, 80},
  box, --canvas for the the world box
  lines --canvas for the lines
}

local players = {}
local myPlayer = "player1"
local curveSegments = 3

--declaration of non-global Initialisation variables
function love.load()
  local width, height = love.graphics.getDimensions()
  resizeWindow(width, height)

  world.lines = love.graphics.newCanvas(world.width, world.height)
  world.box = love.graphics.newCanvas(world.width, world.height)
  love.graphics.setCanvas(world.box)
    love.graphics.setColor(unpack(world.color))
    love.graphics.rectangle("fill", 0, 0, world.width, world.height)
  love.graphics.setCanvas()
  ---[[
  local player1 = {
    name = "player1",
    width = 25,
    color = {198, 13, 75},
    position = {world.width/2, world.height/2}, --position and points are defined this way because it isn't possible to define them in terms of each other within this declaration
    rotation = 0,
    turnSpeed = 2*math.pi,
    baseSpeed = 300,
    speed = 300,
    canvas = love.graphics.newCanvas(world.width, world.height),
    lr = 0,
    ud = 0
  }
  players[player1.name] = player1
  --]]
  ---[[
  local player2 = {
    name = "player2",
    width = 100,
    color = {206, 177, 14},
    position = {world.width/3, world.height/3}, --you don't have to define position in terms of world width since the world width never changes, 1920/3 = 640, 1080/3 = 360
    rotation = math.pi/2,
    turnSpeed = math.pi/2,
    baseSpeed = 200,
    speed = 200,
    canvas = love.graphics.newCanvas(world.width, world.height),
    lr = 0,
    ud = 0
  }
  players[player2.name] = player2
  --]]

  timer = love.timer.getTime() --set timer
end
-------------------------------------------------------------------------------------------------------------------
--Update of game state
function love.update(dt)

  --peroidically get updates for each player's input states lr, ud
  if love.timer.getTime() - timer > updateRate then

    timer = love.timer.getTime() --reset timer

    --update for player1 (server player)
    players[myPlayer].lr = 0
    players[myPlayer].ud = 0
    if love.keyboard.isDown("right") then players[myPlayer].lr = players[myPlayer].lr + 1 end
    if love.keyboard.isDown("left" ) then players[myPlayer].lr = players[myPlayer].lr - 1 end
    if love.keyboard.isDown("up"   ) then players[myPlayer].ud = players[myPlayer].ud + 1 end
    if love.keyboard.isDown("down" ) then players[myPlayer].ud = players[myPlayer].ud - 1 end

    ---[[
    --update for player2 (client player)
    local data, ip, port = udp:receivefrom()
      if data then --only update if there is data to be recieved
        local savedData = data
        ---[[
        while data do --while there are more messages, keep pulling them until you have the most recent one
          savedData = data
          data, ip, port = udp:receivefrom()
        end
        --]]
        local lr, ud = savedData:match("([^,]+),([^,]+)")
        players.player2.lr = tonumber(lr)
        players.player2.ud = tonumber(ud)
      end
    --]]
  end

  for name, player in pairs(players) do

    --calculate movement of each player
    local x = player.position[1] + player.speed*math.cos(player.rotation)*dt
    local y = player.position[2] + player.speed*math.sin(player.rotation)*dt

    --draw lines for that player
    love.graphics.setCanvas(world.lines)
      love.graphics.setColor(unpack(player.color))
      love.graphics.setLineWidth(player.width)

      if player.lr == 0 then
        love.graphics.line(player.position[1], player.position[2], x, y)
      else
        local arcx = player.position[1] + ( player.speed * math.cos(player.rotation + player.lr * math.pi/2) / player.turnSpeed)
        local arcy = player.position[2] + ( player.speed * math.sin(player.rotation + player.lr * math.pi/2) / player.turnSpeed)
        local radius = player.speed / player.turnSpeed
        local rotation = player.rotation - player.lr * math.pi/2
        local newrotation = player.rotation + player.lr * (player.turnSpeed * dt - math.pi/2)
        love.graphics.arc("line", "open", arcx, arcy, radius, rotation, newrotation, curveSegments)
      end

      love.graphics.setColor(255, 255, 255)
    love.graphics.setCanvas()

    --move each player
    player.position[1] = x % world.width
    player.position[2] = y % world.height

    --update rotation and speed according to input
        if player.lr ==  1 then player.rotation = player.rotation + player.turnSpeed * dt
    elseif player.lr == -1 then player.rotation = player.rotation - player.turnSpeed * dt end
    player.speed = 2^player.ud * player.baseSpeed
  end
end
-------------------------------------------------------------------------------------------------------------------
--Drawing of game state
function love.draw()
  --general transformations to fit world to window size
  love.graphics.scale(world.scale, world.scale)
  love.graphics.translate(world.x, world.y)

  --drawing the worldbox
  love.graphics.draw(world.box)

  --drawing the lines
  love.graphics.draw(world.lines)

  --drawing each player
  for name, player in pairs(players) do
    love.graphics.setColor(unpack(player.color))
    love.graphics.circle("fill", player.position[1], player.position[2], player.width/2, 100)
  end

  love.graphics.setColor(255,255,255) --reset colour
  love.graphics.origin() --reset transformations

  --love.graphics.print("Segments: " .. curveSegments, 50, 50, 0, 2, 2)
end
-------------------------------------------------------------------------------------------------------------------
--Input Handler
local keys = {
  escape = love.event.quit,

  space = function()
    love.graphics.setCanvas(world.lines)
      love.graphics.clear()
    love.graphics.setCanvas()
  end,

  --t = function() if curveSegments ~= 1 then curveSegments = curveSegments - 1 end end,
  --y = function() curveSegments = curveSegments + 1 end
}

function love.keypressed(k)
  local action = keys[k]
  if action then return action() end
end
-------------------------------------------------------------------------------------------------------------------
--Co-ordinate System and resolution scaling (requires world object to first be defined with width and height to function propperly)
function resizeWindow(width, height)
  world.scale = math.min(width/world.width, height/world.height)
  world.x = ((width/world.scale)-world.width)/2
  world.y = ((height/world.scale)-world.height)/2
end

function love.resize(width, height)
  resizeWindow(width, height)
end
