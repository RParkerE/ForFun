const std = @import("std");
const renderer = @import("renderer.zig");

pub const Particle = struct {
    x: f32,
    y: f32,
    vx: f32,
    vy: f32,
    char: u8,
    color: renderer.Color,
    lifetime: u32,

    pub fn update(self: *Particle) void {
        self.x += self.vx;
        self.y += self.vy;

        // Wrap around screen boundaries
        if (self.x < 0) self.x = @as(f32, @floatFromInt(renderer.SCREEN_WIDTH)) - 1;
        if (self.x >= @as(f32, @floatFromInt(renderer.SCREEN_WIDTH))) self.x = 0;
        if (self.y < 0) self.y = @as(f32, @floatFromInt(renderer.SCREEN_HEIGHT)) - 1;
        if (self.y >= @as(f32, @floatFromInt(renderer.SCREEN_HEIGHT))) self.y = 0;

        self.lifetime -|= 1; // Saturating subtraction
    }
};

pub const ParticleSystem = struct {
    particles: std.ArrayList(Particle),

    pub fn init(allocator: std.mem.Allocator) ParticleSystem {
        return ParticleSystem{
            .particles = std.ArrayList(Particle).init(allocator),
        };
    }

    pub fn deinit(self: *ParticleSystem) void {
        self.particles.deinit();
    }

    pub fn emit(self: *ParticleSystem, x: f32, y: f32, char: u8, color: renderer.Color) !void {
        const particle = Particle{
            .x = x,
            .y = y,
            .vx = (std.crypto.random.float(f32) - 0.5) * 2,
            .vy = (std.crypto.random.float(f32) - 0.5) * 2,
            .char = char,
            .color = color,
            .lifetime = 20 + std.crypto.random.intRangeAtMost(u32, 0, 40),
        };
        try self.particles.append(particle);
    }

    pub fn update(self: *ParticleSystem) void {
        var i: usize = 0;
        while (i < self.particles.items.len) {
            self.particles.items[i].update();
            if (self.particles.items[i].lifetime == 0) {
                _ = self.particles.swapRemove(i);
            } else {
                i += 1;
            }
        }
    }

    pub fn draw(self: ParticleSystem, screen: *renderer.Screen) void {
        for (self.particles.items) |particle| {
            const x = @as(usize, @intFromFloat(particle.x));
            const y = @as(usize, @intFromFloat(particle.y));
            screen.drawPixel(x, y, renderer.Pixel{ .char = particle.char, .color = particle.color });
        }
    }
};
