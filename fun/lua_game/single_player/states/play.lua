local world = {}
local players = {}
local play = {}

local function createPlayer(color, position, rotation, width, turnSpeed, baseSpeed)
  local player = {
    --visual attributes
    color = color,
    --spawn attributes
    position = position,
    rotation = rotation,
    --game attributes
    width = width,
    turnSpeed = turnSpeed,
    baseSpeed = baseSpeed,
    speed = baseSpeed
  }

  return player
end

play.load = function()

  --Basic world properties
  world.width = gameWidth
  world.height = gameHeight
  world.color = {9, 155, 80}

  --Creating background canvas
  world.box = love.graphics.newCanvas(world.width, world.height)
  love.graphics.setCanvas(world.box)
    love.graphics.setColor(unpack(world.color))
    love.graphics.rectangle("fill", 0, 0, world.width, world.height)
  love.graphics.setCanvas()

  --Initializing empty lines canvas
  world.lines = love.graphics.newCanvas(world.width, world.height)

  --Creating player instances and add them to the players table
  players.player1 = createPlayer({198, 13, 75}, {world.width/2, world.height/2}, 0, 25, 2*math.pi, 300)
  players.player3 = createPlayer({13, 134, 147}, {3*world.width/4, world.height/4}, math.pi/4, 50, math.pi, 250)
  players.player2 = createPlayer({206, 177, 14}, {world.width/3, world.height/3}, math.pi/2, 100, math.pi/2, 200)

  --General game settings
  world.curveSegments = 3
  world.myPlayer = "player1"
end

play.update = function(dt)

  for name, player in pairs(players) do
    --consider input for current player
    local lr, ud = 0, 0
    if name == world.myPlayer then
        if love.keyboard.isDown( "right") then lr = lr + 1 end
        if love.keyboard.isDown( "left" ) then lr = lr - 1 end
        if love.keyboard.isDown( "up" ) then ud = ud + 1 end
        if love.keyboard.isDown( "down") then ud = ud - 1 end
    end

    --calculate movement of each player
    local x = player.position[1] + player.speed * math.cos(player.rotation) * dt
    local y = player.position[2] + player.speed * math.sin(player.rotation) * dt

    --draw lines for that player
    love.graphics.setCanvas(world.lines)
      love.graphics.setColor(unpack(player.color))
      love.graphics.setLineWidth(player.width)

      if lr == 0 then
        love.graphics.line(player.position[1], player.position[2], x, y)
      else
        local arcx = player.position[1] + (player.speed * math.cos(player.rotation + lr * math.pi/2) / player.turnSpeed)
        local arcy = player.position[2] + (player.speed * math.sin(player.rotation + lr * math.pi/2) / player.turnSpeed)
        local radius = player.speed / player.turnSpeed
        local rotation = player.rotation - lr * math.pi/2
        local newrotation = player.rotation + lr * (player.turnSpeed * dt - math.pi/2)
        love.graphics.arc("line", "open", arcx, arcy, radius, rotation, newrotation, world.curveSegments)
      end

      love.graphics.setColor(255, 255, 255)
    love.graphics.setCanvas()

    --move each player
    player.position[1] = x % world.width
    player.position[2] = y % world.height

    --update rotation and speed according to input
    player.rotation = player.rotation + lr * player.turnSpeed * dt
    player.speed = 2^ud * player.baseSpeed
  end

end

play.draw = function()

  --drawing the worldbox
  love.graphics.draw(world.box)

  --drawing the lines
  love.graphics.draw(world.lines)

  --drawing each player
  for name, player in pairs(players) do
    love.graphics.setColor(unpack(player.color))
    love.graphics.circle("fill", player.position[1], player.position[2], player.width/2, 100)
  end

end

play.input = {
  f = function() if world.myPlayer == "player1" then world.myPlayer = "player2" else world.myPlayer = "player1" end end,

  space = function()
    love.graphics.setCanvas(world.lines)
      love.graphics.clear()
    love.graphics.setCanvas()
  end,

  t = function() if world.curveSegments ~= 1 then world.curveSegments = world.curveSegments - 1 end end,
  y = function() world.curveSegments = world.curveSegments + 1 end
}

play.kill = function()

end

return play
