/* base on User.ts from Stendhal -- License: AGPL */
/* https://github.com/arianne/stendhal/blob/master/srcjs/stendhal/entity/User.ts */

class User {
	private readonly soundMan = singletons.getSoundManager();
	private readonly lssMan = singletons.getLoopedSoundSourceManager();

	override minimapStyle = Color.USER;

	override destroy(parent: any) {
		this.onExitZone();
		super.destroy(parent);
	}
}
