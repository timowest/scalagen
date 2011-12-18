package com.mysema.scalagen;

import java.io.Serializable;

import javax.annotation.Nullable;

/**
 * @author tiwe
 *
 * @param <Entity>
 * @param <Id>
 */
public interface ExampleDao<Entity, Id extends Serializable> {
    /**
     * Get the persisted instance with the given id
     *
     * @param id
     * @return
     */
    @Nullable
    Entity getById( Id id );

}