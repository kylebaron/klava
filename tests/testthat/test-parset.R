# (at your option) any later version.
#
# optimhelp is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with optimhelp  If not, see <http://www.gnu.org/licenses/>.

context("test-parset")

test_that("basics functions", {
  foo <- log_par("foo", 1)
  foo2 <- ident_par("foo2", 2)
  p <- parset(foo,foo2)
  expect_true(is.par(foo))
  expect_true(is.parset(p))
  expect_false(is.parset(list(a=1)))
  
  l <- as.list(p)
  expect_is(l,"list")
  expect_equal(l$foo,1)
  expect_equal(l$foo2,2)
  expect_identical(l,list(foo=1,foo2=2))
  
  expect_identical(names(p),c("foo", "foo2"))
  
  expect_identical(as.numeric(p),unlist(l))
})


test_that("log_par transforms correctly", {
  foo <- log_par("foo", 1)
  p <- parset(foo)
  i <- initials(p)
  tr <- trans(p)
  expect_equal(i["foo"],c(foo=log(1)))
  expect_equal(tr$foo,log(1))
})

test_that("ident_par transforms correctly", {
  foo <- ident_par("foo", 1)
  p <- parset(foo)
  i <- initials(p)
  tr <- trans(p)
  expect_equal(i["foo"],c(foo=1))
  expect_equal(tr$foo,1)
})

.logit <- function(x) log(x/(1-x))
test_that("logit_par transforms correctly", {
  foo <- logit_par("foo", 0.75)
  p <- parset(foo)
  i <- initials(p)
  tr <- trans(p)
  expect_equal(i["foo"],c(foo=.logit(0.75)))
  expect_equal(tr$foo,.logit(0.75))
})

test_that("fixed parameter", {
  foo <- ident_par("foo", fixed=TRUE, value=3.3)
  foo2 <- log_par("foo2", fixed=FALSE, value=3.3)
  p <- parset(foo,foo2)
  i <- initials(p)
  expect_equal(length(i),1)
  
  utr <- as.list(untrans(p))
  expect_equal(length(utr),2)
  expect_equal(utr$foo,3.3)
  expect_equal(utr$foo2,3.3)
  
  tr <- as.list(trans(p))
  expect_equal(length(tr),2)
  expect_equal(tr$foo,3.3)
  expect_equal(tr$foo2,log(3.3))
  
})

test_that("duplicate parameter names", {
  foo1 <- log_par("foo", 3.3)
  foo2 <- log_par("foo", 4.4)
  expect_error(parset(foo1,foo2))
  
})

test_that("graft parameters onto parset", {
   p <- parset(log_par("foo", 2),log_par("foo2",10))
   x <- 0.2
   par <- initials(p) + x
   
   pp <- as.list(graft(p,par))
   
   expect_equal(pp$foo,exp(log(2) + x))
   expect_equal(pp$foo2,exp(log(10) + x))

})