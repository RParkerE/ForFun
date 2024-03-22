slint::include_modules!();

use std::io::Read;
use std::str::FromStr;
use slint::{FilterModel, Model, SortModel};

fn save(content: String) -> Result<(), std::io::Error> {
    std::fs::write("db.txt", content)
}

fn load() -> Result<Vec<Task>, std::io::Error> {
    let mut f = std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .read(true)
        .open("db.txt")?;
    let mut content = String::new();
    f.read_to_string(&mut content)?;
    let items: Vec<Task> = content
        .lines()
        .map(|line| line.splitn(2, '\t').collect::<Vec<&str>>())
        .map(|v| (v[0], v[1]))
        .map(|(k, v)| (Task{task: String::from(k).into(), status: bool::from_str(v).unwrap()}))
        .collect();
    Ok(items)
}

fn main() -> Result<(), slint::PlatformError> {
    let ui = AppWindow::new()?;

    ui.on_init_todo({
        let ui_handle = ui.as_weak();
        move || {
            let ui = ui_handle.unwrap();
            let mut tasks: Vec<Task> = ui.get_tasks().iter().collect();
            let items = load();
            tasks.extend(items.expect("Issue with save file"));
            let task_model = std::rc::Rc::new(slint::VecModel::from(tasks));
            ui.set_tasks(task_model.into());
        }
    });

    ui.on_task_toggle({
        let ui_handle = ui.as_weak();
        move |i| {
            let ui = ui_handle.unwrap();
            let mut tasks: Vec<Task> = ui.get_tasks().iter().collect();
            let ind = i as usize;
            tasks[ind].status = !tasks[ind].status;
            let task_model = std::rc::Rc::new(slint::VecModel::from(tasks));
            ui.set_tasks(task_model.into());
        }
    });

    ui.on_add_task({
        let ui_handle = ui.as_weak();
        move |text| {
            let ui = ui_handle.unwrap();
            let mut tasks: Vec<Task> = ui.get_tasks().iter().collect();
            let item: Task = Task{task: text, status: false};
            tasks.extend([item]);
            let task_model = std::rc::Rc::new(slint::VecModel::from(tasks));
            ui.set_tasks(task_model.into());
        }
    });

    ui.on_apply_sort_by_name({
        let ui_handle = ui.as_weak();
        move || {
            let ui = ui_handle.unwrap();
            ui.set_tasks(
                std::rc::Rc::new(SortModel::new(ui.get_tasks(), |lhs, rhs| {
                    lhs.task.to_lowercase().cmp(&rhs.task.to_lowercase())
                })).into(),
            );
        }
    });

    ui.on_apply_hide_done({
        let ui_handle = ui.as_weak();
        move || {
            let ui = ui_handle.unwrap();
            ui.set_tasks(
                std::rc::Rc::new(FilterModel::new(ui.get_tasks(), |e| !e.status)).into(),
            );
        }
    });

    ui.on_save_todo({
        let ui_handle = ui.as_weak();
        move || {
            let ui = ui_handle.unwrap();
            let tasks: Vec<Task> = ui.get_tasks().iter().collect();
            let mut content = String::new();
            for task in tasks {
                let record = format!("{}\t{}\n", task.task, task.status);
                content.push_str(&record)
            }
            _ = save(content);
        }
    });

    ui.run()
}
