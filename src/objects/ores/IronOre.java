package objects.ores;

import objects.templates.WorldObject;

public class IronOre extends WorldObject {

    public IronOre() {
        super("iron_ore", false, true, 2000);
    }

    @Override
    public String[] getResources() {
        return new String[]{"Iron"};
    }

}
