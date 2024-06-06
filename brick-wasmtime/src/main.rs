use std::{
    collections::HashSet,
    env,
    sync::{mpsc::TryRecvError, Arc, Mutex},
};

use anyhow::{bail, Context};
use brick_wasm_backend::compile;
use brick_wasmtime::add_runtime_functions;
use sdl2::{event::Event, keyboard::Keycode, pixels::Color, rect::Rect};
use wasmtime::{Engine, Linker, Module, Store, Val};

fn main() -> anyhow::Result<()> {
    let mut args = env::args();
    args.next();
    let filename = String::leak(args.next().expect("pass a filename"));

    let engine = Engine::default();
    let mut store = Store::new(&engine, ());
    let mut linker = Linker::new(&engine);
    add_runtime_functions(&mut linker)?;

    let (send_draw_command, recv_draw_command) = std::sync::mpsc::channel();
    let down_keys = Arc::new(Mutex::new(HashSet::new()));

    let send_cmd = send_draw_command.clone();
    linker.func_wrap("bindings", "clear", move || {
        send_cmd.send(DrawCommand::Clear).unwrap();
    })?;
    let send_cmd = send_draw_command.clone();
    linker.func_wrap("bindings", "set_color", move |r, g, b, a| {
        send_cmd.send(DrawCommand::SetColor(r, g, b, a)).unwrap();
    })?;
    let send_cmd = send_draw_command.clone();
    linker.func_wrap("bindings", "present", move || {
        send_cmd.send(DrawCommand::Present).unwrap();
    })?;
    let send_cmd = send_draw_command.clone();
    linker.func_wrap("bindings", "fill_rect", move |x, y, w, h| {
        send_cmd.send(DrawCommand::DrawRect(x, y, w, h)).unwrap();
    })?;
    let keys = down_keys.clone();
    linker.func_wrap("bindings", "is_key_down", move |key: i32| {
        let keys = keys.lock().unwrap();
        if keys.contains(&key) {
            1
        } else {
            0
        }
    })?;

    let module = get_module(&engine, filename)?;
    let mut instance = linker.instantiate(&mut store, &module)?;

    let init_func = instance
        .get_func(&mut store, "init")
        .context("failed to find init")?;
    let mut tick_func = instance
        .get_func(&mut store, "tick")
        .context("failed to find tick")?;

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem
        .window("Brick demo", 800, 600)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();
    let mut event_pump = sdl_context.event_pump().unwrap();

    let mut results = [Val::I32(0)];
    init_func.call(&mut store, &[], &mut results)?;

    let ctx_pointer = results[0].clone();

    let mut running = true;
    while running {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => {
                    running = false;
                }
                Event::KeyDown {
                    keycode: Some(Keycode::R),
                    repeat: false,
                    ..
                } => {
                    let memory = instance
                        .get_memory(&mut store, "memory")
                        .context("failed to find memory")?;
                    let data = memory.data(&store).to_vec();
                    let module = get_module(&engine, filename)?;
                    instance = linker.instantiate(&mut store, &module)?;
                    tick_func = instance
                        .get_func(&mut store, "tick")
                        .context("failed to find tick")?;
                    let memory = instance
                        .get_memory(&mut store, "memory")
                        .context("failed to find memory")?;
                    memory.data_mut(&mut store).copy_from_slice(&data[..]);
                    println!("reloaded!");
                }
                Event::KeyDown {
                    keycode: Some(keycode),
                    repeat: false,
                    ..
                } => {
                    let key = keycode as i32;
                    let mut keys = down_keys.lock().unwrap();
                    keys.insert(key);
                }
                Event::KeyUp {
                    keycode: Some(keycode),
                    repeat: false,
                    ..
                } => {
                    let key = keycode as i32;
                    let mut keys = down_keys.lock().unwrap();
                    keys.remove(&key);
                }
                _ => {}
            }
        }

        tick_func.call(&mut store, &[ctx_pointer.clone()], &mut [])?;

        loop {
            let event = match recv_draw_command.try_recv() {
                Ok(event) => event,
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => bail!("Event loop disconnected"),
            };
            match event {
                DrawCommand::Clear => canvas.clear(),
                DrawCommand::Present => canvas.present(),
                DrawCommand::SetColor(r, g, b, a) => {
                    canvas.set_draw_color(Color::RGBA(r as u8, g as u8, b as u8, a as u8));
                }
                DrawCommand::DrawRect(x, y, w, h) => {
                    canvas
                        .fill_rect(Rect::new(x, y, w as u32, h as u32))
                        .unwrap();
                }
            }
        }
    }

    Ok(())
}

fn get_module(engine: &Engine, filename: &'static str) -> anyhow::Result<Module> {
    let contents = std::fs::read_to_string(filename)?;
    let binary = compile("main", filename, contents, true)?;
    Module::from_binary(engine, binary.as_slice())
}

enum DrawCommand {
    Clear,
    Present,
    SetColor(i32, i32, i32, i32),
    DrawRect(i32, i32, i32, i32),
}
