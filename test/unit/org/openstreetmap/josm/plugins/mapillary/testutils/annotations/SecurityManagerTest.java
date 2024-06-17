// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.testutils.annotations;

import java.security.Permission;

import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.extension.ExtensionContext;

/**
 * Used to indicate a security manager should be installed for a test.
 * This is mostly useful for non-regression tests for WebStart related issues.
 *
 * @author Taylor Smock
 */
@ExtendWith(SecurityManagerTest.SecurityManagerExtension.class)
public @interface SecurityManagerTest {
    class SecurityManagerExtension implements AfterEachCallback, BeforeEachCallback {
        private static class TestSecurityManager extends SecurityManager {
            @Override
            public void checkPermission(Permission perm) {
                if (perm instanceof RuntimePermission && "setSecurityManager".equals(perm.getName())) {
                    StackTraceElement stackTrace = Thread.currentThread().getStackTrace()[1];
                    if (SecurityManagerTest.SecurityManagerExtension.class.getCanonicalName()
                        .equals(stackTrace.getClassName())) {
                        return;
                    }
                }
                super.checkPermission(perm);
            }
        }

        @Override
        public void afterEach(ExtensionContext context) {
            SecurityManager sm = System.getSecurityManager();
            if (sm instanceof TestSecurityManager) {
                System.setSecurityManager(null);
            }
        }

        @Override
        public void beforeEach(ExtensionContext context) {
            if (System.getSecurityManager() == null) {
                TestSecurityManager securityManager = new TestSecurityManager();
                System.setSecurityManager(securityManager);
            }
        }
    }
}
