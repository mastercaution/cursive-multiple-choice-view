// Minimal example with a String selection

use cursive::{view::Nameable, views::Dialog};
use cursive_multiple_choice_view::MultipleChoiceView;

fn main() {
	let mut siv = cursive::default();

	let multiple_choice = MultipleChoiceView::new()
		.item_str("Option 1")
		.item_str("Option 2")
		.item_str("Option 3")
		.item_str("Option 4")
		.with_inactive_highlight(false)
		.with_name("multiple_choice_view");
	
	siv.add_layer(Dialog::around(multiple_choice)
		.title("Enable options")
		.button("Submit", |s| {
			// Getting the current choices from the view
			let choice = s.call_on_name("multiple_choice_view", |v: &mut MultipleChoiceView| {
				v.get_choice()
			}).unwrap();

			// Construct output string. Not pretty but minimal.
			let mut choice_str = String::new();
			for c in choice {
				choice_str.push(' ');
				choice_str.push_str(c.as_str());
				choice_str.push(',');
			}

			s.add_layer(Dialog::text(choice_str)
				.button("Quit", |s| s.quit())
			);
		})
	);

	siv.run();
}