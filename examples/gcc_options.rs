use cursive::{
	view::{Nameable, Resizable},
	views::{Dialog, LinearLayout, PaddedView, TextView},
	Cursive
};
use cursive_multiple_choice_view::MultipleChoiceView;



fn main() {
	let mut siv = cursive::default();

	// Initial content is a little cheated ...
	let preview = TextView::new("Preview: \"gcc -O2 -flto -Wall missing.c\"").with_name("preview");

	let options = MultipleChoiceView::new()
		.item_str_with_choice("-O2", true)
		.item_str_with_choice("-flto", true)
		.item_str_with_choice("-DDEBUG", false)
		.item_str_with_choice("-Wall", true)
		.item_str_with_choice("-Werror", false)
		.with_inactive_highlight(false)
		.on_submit(|s, _: &str| {
			let command = create_command(s);
			s.call_on_name("preview", |v: &mut TextView| {
				v.set_content(format!("Preview: \"{}\"", command));
			});
		})
		.with_name("multiple_choice_view");

	let layout = LinearLayout::vertical()
		.child(PaddedView::lrtb(0, 0, 0, 1, preview))
		.child(options);
	
	siv.add_layer(Dialog::around(layout)
		.title("Choose enabled options:")
		.button("Ok", |s| {
			let command = create_command(s);
			s.pop_layer();
			s.add_layer(Dialog::text(command)
				.title("Command")
				.button("Finish", |s| s.quit())
			);
		})
		.min_width(60)
	);

	siv.run();
}

fn create_command(s: &mut Cursive) -> String {
	// Getting the choice from the view
	let choice = s.call_on_name("multiple_choice_view", |v: &mut MultipleChoiceView| {
		v.get_choice()
	}).unwrap();
	
	let mut command = String::from("gcc");
	for c in choice {
		command.push(' ');
		command.push_str(c.as_str());
	}
	command.push_str(" missing.c");
	return command;
}