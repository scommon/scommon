/*
*  java-sandbox
*  Copyright (c) 2012 datenwerke Jan Albrecht
*  http://www.datenwerke.net
*  
*  This file is part of the java-sandbox: https://sourceforge.net/p/dw-sandbox/
*
*
*  This program is free software: you can redistribute it and/or modify
*  it under the terms of the GNU General Public License as published by
*  the Free Software Foundation, either version 3 of the License, or
*  (at your option) any later version.

*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
*/

package net.datenwerke.sandbox.permissions;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * A file permission checking on equality.
 * 
 * @author Arno Mittelbach
 *
 */
public class FileEqualsPermission implements FilePermission {

	/**
	 * 
	 */
	private static final long serialVersionUID = 7056195473211397460L;

    private final Path mask;
	private boolean negate = false;
	
	public FileEqualsPermission(String mask) {
		this(mask,false);
	}

    public FileEqualsPermission(String mask, boolean negate) {
        this(Paths.get(mask), negate);
    }

    public FileEqualsPermission(Path mask, boolean negate) {
        this.mask = mask.toAbsolutePath();
        this.negate = negate;
    }
	
	@Override
	public boolean testPermission(String file) {
        final String sanitized_file;
        if (file.endsWith("-") || file.endsWith("*"))
            sanitized_file = file.substring(0, file.length() - 1);
        else
            sanitized_file = file;

        final Path p = new File(sanitized_file).getAbsoluteFile().toPath();
        return mask.equals(p) ^ negate;
	}
	
	@Override
	public FileEqualsPermission clone() {
		return new FileEqualsPermission(mask, negate);
	}

	@Override
	public String toString() {
		return "FileEqualsPermission [mask=" + mask + ", negate=" + negate
				+ "]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((mask == null) ? 0 : mask.hashCode());
		result = prime * result + (negate ? 1231 : 1237);
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		FileEqualsPermission other = (FileEqualsPermission) obj;
		if (mask == null) {
			if (other.mask != null)
				return false;
		} else if (!mask.equals(other.mask))
			return false;
		if (negate != other.negate)
			return false;
		return true;
	}
	
	
}
